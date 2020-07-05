using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.MSBuild;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Policy;
using System.Threading.Tasks;

namespace CodeCleanerCLI
{
    class Program
    {
        static async Task Main(string[] args)
        {
            //var solutionFile = args[0];
            var targetFile = @"..\..\..\..\DemoProject\ConsoleApp\ConsoleApp.sln";
            //var targetFile = @"D:\Projects\doggy\doggy.csproj";
            bool isSolution = targetFile.EndsWith(".sln");
            Console.WriteLine($"target[{(isSolution ? "solution" : "project")}]: {targetFile}");

            MSBuildLocator.RegisterDefaults();
            using MSBuildWorkspace workspace = MSBuildWorkspace.Create();
            workspace.WorkspaceFailed += Workspace_WorkspaceFailed;
            Solution solution = 
                isSolution ?
             await workspace.OpenSolutionAsync(targetFile)
             :
             (await workspace.OpenProjectAsync(targetFile)).Solution;

            HashSet<ISymbol> usedTypes = new HashSet<ISymbol>();
            HashSet<ISymbol> usedBySelfTypes = new HashSet<ISymbol>();
            HashSet<ISymbol> notUsedTypes = new HashSet<ISymbol>();
            HashSet<ISymbol> usedByNotUsedTypeTypes = new HashSet<ISymbol>();

            HashSet<ISymbol> specialTypes = new HashSet<ISymbol>();

            Dictionary<ISymbol, List<ReferencedSymbol>> typeRefs = new Dictionary<ISymbol, List<ReferencedSymbol>>();

            await foreach(var type in solution.GetAllNamedType())
            {
                var refs = await SymbolFinder.FindReferencesAsync(type, solution);
                typeRefs.Add(type, refs.ToList());
            }
            //第一轮分析
            foreach(var (type, refs) in typeRefs)
            {
                if (refs.Any(r => r.Locations.Any()))
                {
                    if (refs.All(r => r.Locations.All(l => l.IsInDefinition(type))))
                    {
                        usedBySelfTypes.Add(type);
                    }
                    else
                    {
                        usedTypes.Add(type);
                    }
                }
                else
                {
                    notUsedTypes.Add(type);
                }
            }
            /// 分析： 存在特殊标记的类型（可能被运行时实例化）/继承特殊基类的类型/实现特殊接口的类型
            foreach (INamedTypeSymbol type in notUsedTypes.Concat(usedBySelfTypes))
            {
                if (type.GetAttributes().Any())
                {
                    specialTypes.Add(type);
                    continue;
                }
                if (type.BaseType != null && new []{ "PageModel",  }.Contains(type.BaseType.Name))
                {
                    specialTypes.Add(type);
                    continue;
                }
                if(type.AllInterfaces.Any(i => new [] { "ICommand", "ICommandHandler" }.Contains(i.Name)))
                {
                    specialTypes.Add(type);
                    continue;
                }
            }
            notUsedTypes.ExceptWith(specialTypes);
            usedBySelfTypes.ExceptWith(specialTypes);

            /// 分析：类型未被引用，但成员被引用（目前已知扩展方法会这样）
            /// 处理：查找成员引用。如果存在被usedType引用的，则认为是usedType
            foreach (INamedTypeSymbol type in notUsedTypes)
            {
                var methods = type.GetMembers().Where(m => m is IMethodSymbol ms).Cast<IMethodSymbol>();
                foreach(var m in methods)
                {
                    if (m.IsExtensionMethod)
                    {
                        var refs = await SymbolFinder.FindCallersAsync(m, solution);
                        if (refs.Any(r => usedTypes.Concat(specialTypes).Contains(r.CallingSymbol.GetDefineType())))
                        {
                            specialTypes.Add(type);
                            break;
                        }
                    }
                    //分析 Main 函数
                    else if(m.Name == "Main" && m.IsStatic && m.Parameters.FirstOrDefault()?.Type is IArrayTypeSymbol ats && ats.ElementType.SpecialType == SpecialType.System_String)
                    {
                        specialTypes.Add(type);
                        break;
                    }
                }
            }
            notUsedTypes.ExceptWith(specialTypes);

            /// 第3轮分析：被notUsed引用的类型
            foreach(var type in usedTypes)
            {
                var refs = typeRefs[type];
                
                var usages = refs.SelectMany(r => r.Locations.Select(l =>
                {
                    if (l.IsInDefinition(type)) return UsageType.UsedBySelf;
                    if (notUsedTypes.Any(t => l.IsInDefinition(t))) return UsageType.UsedBySomeoneWhoWasNotUsed;
                    return UsageType.Used;
                }));
                if(usages.All(usage => usage == UsageType.UsedBySelf || usage == UsageType.UsedBySomeoneWhoWasNotUsed))
                {
                    usedByNotUsedTypeTypes.Add(type);
                }

            }
            usedTypes.ExceptWith(usedByNotUsedTypeTypes);
            Console.WriteLine($"[notUsedTotal]{notUsedTypes.Count + usedBySelfTypes.Count + usedByNotUsedTypeTypes.Count}");
            Console.WriteLine($"[usedBySelf]-------{usedBySelfTypes.Count}---------");
            foreach (var type in usedBySelfTypes)
            {
                Console.WriteLine($"[usedBySelf]{type.Name}");
            }
            Console.WriteLine($"[usedByNotUsed]-------{usedByNotUsedTypeTypes.Count}---------");
            foreach (var type in usedByNotUsedTypeTypes)
            {
                Console.WriteLine($"[usedByNotUsed]{type.Name}");
            }
            Console.WriteLine($"[notUsed]-------{notUsedTypes.Count}---------");
            foreach (var type in notUsedTypes)
            {
                Console.WriteLine($"[notUsed]{type.Name}");
            }
            Console.WriteLine("analysis finish.");
            
            Console.WriteLine("input 'rm' to exec remove action, otherwise quit.");
            var input = Console.ReadLine();
            if (!input.StartsWith("rm")) return;

            solution = await solution.RemoveSymbolsAsync(notUsedTypes.Concat(usedBySelfTypes).Concat(usedByNotUsedTypeTypes));
            var ret = workspace.TryApplyChanges(solution);
            Console.WriteLine($"save changes, result: {ret}");
            
            Console.ReadKey();
        }

        private static void Workspace_WorkspaceFailed(object sender, WorkspaceDiagnosticEventArgs e)
        {
            Console.WriteLine($"[error]{e.Diagnostic.Message}");
        }

        enum UsageType
        {
            Used,
            UsedBySelf,
            UsedBySomeoneWhoWasNotUsed,
        }
    }

    static class RoslynExtensions
    {
        public static async IAsyncEnumerable<INamedTypeSymbol> GetAllNamedType(this Solution solution)
        {
            foreach (Document d in solution.Projects.SelectMany(p => p.Documents))
            {
                SemanticModel m = await d.GetSemanticModelAsync();
                var syntaxRoot = await d.GetSyntaxRootAsync();
                var nodes = syntaxRoot.DescendantNodes().ToArray();
                foreach (var c in nodes.OfType<TypeDeclarationSyntax>())
                {
                    yield return m.GetDeclaredSymbol(c);
                }
            }
        }
        
        public static bool IsInDefinition(this ReferenceLocation location, ISymbol symbol)
        {
            return symbol.DeclaringSyntaxReferences.Any(d => d.Span.Contains(location.Location.SourceSpan) && location.Document.FilePath == d.SyntaxTree.FilePath);
        }

        public static INamedTypeSymbol GetDefineType(this ISymbol s)
        {
            if (s is INamedTypeSymbol its) return its;
            return s.ContainingType;
        }

        //not working when invoked with multiple cached symbols
        public static async Task<Solution> RemoveSymbolAsync(this Solution solution, ISymbol s)
        {
            foreach (var declare in s.DeclaringSyntaxReferences)
            {
                var node = await declare.GetSyntaxAsync();
                if (!node.GetLocation().IsInSource) continue;
                var docId = solution.GetDocumentId(node.SyntaxTree);
                if (docId == null) //maybe deleted before (subtype who's parent has been removed?)
                {
                    continue;
                }
                var root = await node.SyntaxTree.GetRootAsync();
                var newRoot = root.RemoveNode(node, SyntaxRemoveOptions.KeepNoTrivia);
                solution = solution.WithDocumentSyntaxRoot(docId, newRoot);
            }

            return solution;
        }

        public static async Task<Solution> RemoveSymbolsAsync(this Solution solution, IEnumerable<ISymbol> s)
        {
            var symbolGroups = s.SelectMany(s => s.DeclaringSyntaxReferences)
                .Select(d => d.GetSyntax())
                .GroupBy(node => solution.GetDocumentId(node.SyntaxTree)).ToArray();
            foreach (var symbolsPerDoc in symbolGroups)
            {
                var root = await symbolsPerDoc.First().SyntaxTree.GetRootAsync();
                root = root.RemoveNodes(symbolsPerDoc, SyntaxRemoveOptions.KeepNoTrivia);
                solution = solution.WithDocumentSyntaxRoot(symbolsPerDoc.Key, root);
            }

            return solution;
        }
    }
}
