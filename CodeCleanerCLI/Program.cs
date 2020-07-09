using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.MSBuild;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Security.Policy;
using System.Threading.Tasks;
using Microsoft.Build.Logging;

namespace CodeCleanerCLI
{
    class Program
    {
        private static FileLogger _logger;
        static async Task Main(string[] args)
        {
            //var solutionFile = args[0];
            //var targetFile = @"..\..\..\..\DemoProject\ConsoleApp\ConsoleApp.sln";
            //var targetFile = @"D:\Projects\doggy\doggy.csproj";
            // Console.WriteLine("input project or solution file path:");
            // var targetFile = Console.ReadLine();
            var targetFile = "/Volumes/hjdclient.3d/hjdclient.3d/Assembly-CSharp.csproj";
            var logFile = "/Volumes/SAMSUNG_T5/Logs/CodeCleanerCLI.log";
            _logger = new FileLogger(logFile) {WriteToConsole = true};
            
            bool isSolution = targetFile.EndsWith(".sln");
            _logger.WriteLine($"target[{(isSolution ? "solution" : "project")}]: {targetFile}");

            MSBuildLocator.RegisterDefaults();
            using MSBuildWorkspace workspace = MSBuildWorkspace.Create();
            workspace.WorkspaceFailed += Workspace_WorkspaceFailed;
            
            List<Project> projects = new List<Project>();
            Solution solution = null;
            if (isSolution)
            {
                solution = await workspace.OpenSolutionAsync(targetFile);
                projects.AddRange(solution.Projects);
            }
            else
            {
                projects.Add((await workspace.OpenProjectAsync(targetFile)));
                solution = projects[0].Solution;
            }
            
            HashSet<ISymbol> usedTypes = new HashSet<ISymbol>();
            HashSet<ISymbol> usedBySelfTypes = new HashSet<ISymbol>();
            HashSet<ISymbol> notUsedTypes = new HashSet<ISymbol>();
            HashSet<ISymbol> usedByNotUsedTypeTypes = new HashSet<ISymbol>();

            HashSet<ISymbol> specialTypes = new HashSet<ISymbol>();

            Dictionary<ISymbol, List<ReferencedSymbol>> typeRefs = new Dictionary<ISymbol, List<ReferencedSymbol>>();

            var unityTypes = new[] {"ScriptableObject", "MonoBehaviour", "Graphic", "BaseMeshEffect" };
            var ignorePaths = new[] { "Assets/Components3rd/" };
            var unityPreSymbols = new[] { "UNITY_ANDROID", "UNITY_IOS", "UNITY_STANDALONE_WIN", "UNITY_STANDALONE_OSX"};
            var parseOptions = new CSharpParseOptions(
                LanguageVersion.Latest,
                DocumentationMode.Parse,
                SourceCodeKind.Regular,
                unityPreSymbols
            );
            foreach (var project in projects)
            {
                var np = project.WithParseOptions(parseOptions);
                await foreach(var type in np.GetAllNamedType())
                {
                    if (typeRefs.ContainsKey(type)) continue;
                    //if(type.Name == "GoTo2DButtonView") Debugger.Break();
                    //预处理： 特殊标记类型直接进入usedType，不再搜索引用
                    if (type.IsInheritFromAny(unityTypes)
                    || ignorePaths.Any(path => type.GetDefineFile().Contains(path))
                    )
                    {
                        specialTypes.Add(type);
                        continue;
                    }
                    
                    var refs = await SymbolFinder.FindReferencesAsync(type, solution);
                    typeRefs.Add(type, refs.ToList());
                }
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
            // foreach (INamedTypeSymbol type in notUsedTypes.Concat(usedBySelfTypes))
            // {
            //     if (type.GetAttributes().Any())
            //     {
            //         specialTypes.Add(type);
            //         continue;
            //     }
            //     if (type.BaseType != null && new []{ "PageModel",  }.Contains(type.BaseType.Name))
            //     {
            //         specialTypes.Add(type);
            //         continue;
            //     }
            //     if(type.AllInterfaces.Any(i => new [] { "ICommand", "ICommandHandler" }.Contains(i.Name)))
            //     {
            //         specialTypes.Add(type);
            //         continue;
            //     }
            // }
            // notUsedTypes.ExceptWith(specialTypes);
            // usedBySelfTypes.ExceptWith(specialTypes);

            /// 分析：类型未被引用，但成员被引用（目前已知扩展方法会这样）
            /// 处理：查找成员引用。如果存在被usedType引用的，则认为是usedType

            var comparer = SymbolEqualityComparer.Default;
            
            foreach (INamedTypeSymbol type in notUsedTypes)
            {
                if(type.Name == "CardInfoFormater") Debugger.Break();
                var methods = type.GetMembers().Where(m => m is IMethodSymbol ms).Cast<IMethodSymbol>();
                foreach(var m in methods)
                {
                    if (m.IsExtensionMethod)
                    {
                        var refs = await SymbolFinder.FindCallersAsync(m, solution);
                        if (refs.Any(r => usedTypes.Concat(specialTypes).Contains(r.CallingSymbol.GetDefineType(), comparer)))
                        {
                            specialTypes.Add(type);
                            break;
                        }
                    }
                    // //分析 Main 函数
                    // else if(m.Name == "Main" && m.IsStatic && m.Parameters.FirstOrDefault()?.Type is IArrayTypeSymbol ats && ats.ElementType.SpecialType == SpecialType.System_String)
                    // {
                    //     specialTypes.Add(type);
                    //     break;
                    // }
                }
            }
            notUsedTypes.ExceptWith(specialTypes);

            /// 第3轮分析：被notUsed引用的类型
            foreach(var type in usedTypes)
            {
                if (!typeRefs.TryGetValue(type, out var refs)) continue;
                
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
            _logger.WriteLine($"[notUsedTotal]{notUsedTypes.Count + usedBySelfTypes.Count + usedByNotUsedTypeTypes.Count}");
            _logger.WriteLine($"[usedBySelf]-------{usedBySelfTypes.Count}---------");
            foreach (var type in usedBySelfTypes)
            {
                _logger.WriteLine($"[usedBySelf]{type.Name}");
            }
            _logger.WriteLine($"[usedByNotUsed]-------{usedByNotUsedTypeTypes.Count}---------");
            foreach (var type in usedByNotUsedTypeTypes)
            {
                _logger.WriteLine($"[usedByNotUsed]{type.Name}");
            }
            _logger.WriteLine($"[notUsed]-------{notUsedTypes.Count}---------");
            foreach (var type in notUsedTypes)
            {
                _logger.WriteLine($"[notUsed]{type.Name}");
            }
            _logger.WriteLine("analysis finish.");
            
            Console.WriteLine("input 'rm' to exec remove action, otherwise quit.");
            var input = Console.ReadLine();
            if (!input.StartsWith("rm")) return;

            solution = await solution.RemoveSymbolsAsync(notUsedTypes.Concat(usedBySelfTypes).Concat(usedByNotUsedTypeTypes));
            var ret = workspace.TryApplyChanges(solution);
            _logger.WriteLine($"save changes, result: {ret}");
            
            _logger.Dispose();
            Console.ReadKey();
        }

        private static void Workspace_WorkspaceFailed(object sender, WorkspaceDiagnosticEventArgs e)
        {
            _logger.WriteLine($"[error]{e.Diagnostic.Message}");
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
        
        public static async IAsyncEnumerable<INamedTypeSymbol> GetAllNamedType(this Project project)
        {
            foreach (Document d in project.Documents)
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

        public static string GetDefineFile(this ISymbol symbol)
        {
            return symbol.DeclaringSyntaxReferences[0].SyntaxTree.FilePath;
        }

        public static bool IsInheritFromAny(this INamedTypeSymbol symbol, string[] baseTypeNames)
        {
            while (symbol.BaseType != null)
            {
                if (baseTypeNames.Contains($"{symbol.BaseType.Name}")) return true;
                symbol = symbol.BaseType;
            }

            
            return false;
        }
    }
}
