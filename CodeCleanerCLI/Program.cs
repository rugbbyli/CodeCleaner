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
            //var targetFile = @"D:\Projects\2020\CodeCleaner\DemoProject\ConsoleApp\ConsoleApp.sln";
            var targetFile = @"D:\Projects\doggy\doggy.csproj";
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

            await foreach(var type in GetAllNamedType(solution))
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

                //标记为used，但是refs为空，可能是扩展方法类
                //if (refs.Sum(r => r.Locations.Count()) == 0) continue;

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
            //foreach (var type in usedTypes)
            //{
            //    Console.WriteLine($"[used]{type.Name}");
            //}
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
            Console.WriteLine("finish");
            Console.ReadKey();
        }

        private static void Workspace_WorkspaceFailed(object sender, WorkspaceDiagnosticEventArgs e)
        {
            Console.WriteLine($"[error]{e.Diagnostic.Message}");
        }

        private static async IAsyncEnumerable<INamedTypeSymbol> GetAllNamedType(Solution solution)
        {
            foreach (Document d in solution.Projects.SelectMany(p => p.Documents))
            {
                SemanticModel m = await d.GetSemanticModelAsync();
                var syntaxRoot = await d.GetSyntaxRootAsync();
                var nodes = syntaxRoot.DescendantNodes().ToArray();
                foreach (var c in nodes.OfType<TypeDeclarationSyntax>())
                {
                    yield return m.GetDeclaredSymbol(c) as INamedTypeSymbol;
                }
            }
        }

        private static async IAsyncEnumerable<INamedTypeSymbol> GetAllNamedType2(Solution solution)
        {
            foreach (Project p in solution.Projects)
            {
                var compilation = await p.GetCompilationAsync();

                //foreach (var @class in compilation.GlobalNamespace.GetNamespaceMembers().SelectMany(x => x.GetMembers()))
                //{
                //    Console.WriteLine(@class.Name);
                //    Console.WriteLine(@class.ContainingNamespace.Name);
                //}

                var classVisitor = new ClassVirtualizationVisitor();

                foreach (var syntaxTree in compilation.SyntaxTrees)
                {
                    var node = classVisitor.Visit(syntaxTree.GetRoot());
                    yield return compilation.GetSemanticModel(syntaxTree).GetDeclaredSymbol(node) as INamedTypeSymbol;
                }
            }
        }

        class ClassVirtualizationVisitor : CSharpSyntaxRewriter
        {
            public ClassVirtualizationVisitor()
            {
                //Classes = new List<ClassDeclarationSyntax>();
            }

            //public List<ClassDeclarationSyntax> Classes { get; set; }

            public override SyntaxNode VisitClassDeclaration(ClassDeclarationSyntax node)
            {
                node = (ClassDeclarationSyntax)base.VisitClassDeclaration(node);
                //Classes.Add(node); // save your visited classes
                return node;
            }
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
        public static bool IsInDefinition(this ReferenceLocation location, ISymbol symbol)
        {
            return symbol.DeclaringSyntaxReferences.Any(d => d.Span.Contains(location.Location.SourceSpan) && location.Document.FilePath == d.SyntaxTree.FilePath);
        }

        public static INamedTypeSymbol GetDefineType(this ISymbol s)
        {
            if (s is INamedTypeSymbol its) return its;
            return s.ContainingType;
        }
    }
}
