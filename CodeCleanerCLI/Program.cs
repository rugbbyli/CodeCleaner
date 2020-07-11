using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.MSBuild;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace CodeCleanerCLI
{
    static class Program
    {
        private static FileLogger _logger;
        
        private static readonly string[] unityWinSymbols = { "UNITY_STANDALONE", "UNITY_STANDALONE_WIN" };
        private static readonly string[] unityAndroidSymbols = { "UNITY_ANDROID" };
        private static readonly string[] unityiOSSymbols = { "UNITY_IPHONE", "UNITY_IOS" };
        private static readonly string[] unityMacSymbols = { "UNITY_STANDALONE", "UNITY_STANDALONE_OSX" };
        private static readonly string[] unityEditorSymbols = { "UNITY_EDITOR", "UNITY_EDITOR_64", "UNITY_EDITOR_WIN", "UNITY_EDITOR_OSX", "UNITY_EDITOR_LINUX" };

        private static readonly string[] ignorePaths =
        {
            $"Assets{Path.DirectorySeparatorChar}Components3rd{Path.DirectorySeparatorChar}",
            $"Assets{Path.DirectorySeparatorChar}Standard Assets{Path.DirectorySeparatorChar}",
        };

        private static IEnumerable<(string[] add, string[] rm)> EnumUnityPlatformSymbols()
        {
            return new[] {
                (unityAndroidSymbols, unityEditorSymbols), 
                (unityWinSymbols, unityEditorSymbols), 
                (unityiOSSymbols, unityEditorSymbols), 
                (unityMacSymbols, unityEditorSymbols), 
                (unityEditorSymbols, new string[0])
            };
        }

        private static IEnumerable<string> RemoveUnityEditorSymbols(IEnumerable<string> symbols)
        {
            return symbols.Where(s => !s.StartsWith("UNITY_EDITOR"));
        }
        
        static async Task Main(string[] args)
        {
            //var solutionFile = args[0];
            //var targetFile = @"D:\Projects\doggy\doggy.csproj";
            // Console.WriteLine("input project or solution file path:");
            // var targetFile = Console.ReadLine();
            var logFile = "Logs/CodeCleanerCLI.log";
            _logger = new FileLogger(logFile) {WriteToConsole = true};
            
            // var solutionFile = @"..\..\..\..\DemoProject\ConsoleApp\ConsoleApp.sln";
            // var projectNames = new string[0];
            var solutionFile = "/Volumes/hjdclient.3d/hjdclient.3d/integrated.sln";
            var projectNames = new[] {"Assembly-CSharp", "Assembly-CSharp-firstpass"};
            _logger.WriteLine($"solution: {solutionFile}");

            MSBuildLocator.RegisterDefaults();
            using MSBuildWorkspace workspace = MSBuildWorkspace.Create();
            workspace.WorkspaceFailed += Workspace_WorkspaceFailed;

            List<Project> projects = new List<Project>();
            Solution solution = await workspace.OpenSolutionAsync(solutionFile);
            if (projectNames.Any())
            {
                projects.AddRange(projectNames.Select(name => solution.Projects.FirstOrDefault(p => p.Name == name)).Where(p => p != null).Distinct());
            }
            else
            {
                projects.AddRange(solution.Projects);
            }

            var solutionFolder = Path.GetDirectoryName(Path.GetFullPath(solutionFile));
            bool FilterDoc(Document doc)
            {
                return doc.FilePath!.StartsWith(solutionFolder!) && !ignorePaths.Any(path => doc.FilePath!.Contains(path));
            }

            var allPlatformSymbols = EnumUnityPlatformSymbols();
            
            Console.WriteLine("scan and remove not used type definition? (Y/N)");
            if (InputContinue())
            {
                _logger.WriteLine($"==================remove not used type=====================");
                solution = await RemoveUnUsedTypes(solution, projects, allPlatformSymbols, FilterDoc);
                var ret = workspace.TryApplyChanges(solution);
                _logger.WriteLine($"finish and save changes, result: {ret}");
            }

            Console.WriteLine("scan and remove files without type definition? (Y/N)");
            if (InputContinue())
            {
                _logger.WriteLine($"==================remove not used files=====================");
                solution = await CheckFilesAndRemove(solution, projects, allPlatformSymbols, FilterDoc);
                var ret = workspace.TryApplyChanges(solution);
                _logger.WriteLine($"finish and save changes, result: {ret}");}
            
            _logger.Dispose();
            Console.ReadKey();
        }

        private static async Task<Solution> RemoveUnUsedTypes(Solution solution, IEnumerable<Project> projects, IEnumerable<(string[] add, string[] rm)> platforms, Func<Document, bool> documentFilter)
        {
            var comparer = new TypeSymbolEqualityComparer();
            
            HashSet<ISymbol> usedTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> usedBySelfTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> notUsedTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> usedByNotUsedTypeTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> specialTypes = new HashSet<ISymbol>(comparer);

            Dictionary<ISymbol, List<ReferencedSymbol>> typeRefs = new Dictionary<ISymbol, List<ReferencedSymbol>>(comparer);

            //var unityTypes = new[] {"ScriptableObject", "MonoBehaviour", "Graphic", "BaseMeshEffect" };
            var unityTypes = new[] { "UnityEngine.Object" };

            var platformSolutions = platforms.Select(p => (p, solution.WithChangeSymbols(p.add, p.rm))).ToArray();

            _logger.WriteLine($"analysis all type references at {DateTime.Now}");
            //获取全部类型
            foreach (var (platform, platformSolution) in platformSolutions)
            {
                foreach (var project in projects)
                {
                    _logger.WriteLine($"analysis all type references from project {project.Name} with platform {string.Join('+',platform)} at {DateTime.Now}");
                    var np = platformSolution.Projects.First(p => p.Name == project.Name);
                    var taskGroup = new Dictionary<INamedTypeSymbol, Task<(INamedTypeSymbol Type, IEnumerable<ReferencedSymbol> Result)>>(comparer);
                    await foreach(var type in np.GetAllNamedType(documentFilter))
                    {
                        //if (typeRefs.ContainsKey(type)) continue;
                    
                        //预处理： 特殊标记类型直接进入usedType，不再搜索引用
                        if (type.IsInheritFromAny(unityTypes))
                        {
                            //_logger.WriteLine($"{type.Name}: special.");
                            specialTypes.Add(type);
                            continue;
                        }

                        if (taskGroup.ContainsKey(type)) continue;
                        taskGroup.Add(type, SymbolFinder.FindReferencesAsync(type, platformSolution).ContinueWith(t => (Type:type, t.Result)));
                    }

                    var results = await Task.WhenAll(taskGroup.Values);
                    foreach (var result in results)
                    {
                        if(result.Type.Name == "AndroidNotificationService") Debugger.Break();
                        if (typeRefs.TryGetValue(result.Type, out var refs))
                        {
                            refs.AddRange(result.Result);
                        }
                        else
                        {
                            typeRefs.Add(result.Type, result.Result.ToList());
                        }
                    }
                }
            }
            _logger.WriteLine($"analysis all type references finish, scan {typeRefs.Count} types.");

            _logger.WriteLine($"analysis usage at {DateTime.Now}");
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

            
            _logger.WriteLine($"analysis extension methods at {DateTime.Now}");
            foreach (var (platform, platformSolution) in platformSolutions)
            {
                foreach (INamedTypeSymbol type in notUsedTypes)
                {
                    //if(type.Name == "DataCenterExtension") Debugger.Break();
                    //todo: not very safe, GetMembers may missing some members as it was created in one platform
                    var methods = type.GetMembers().Where(m => m is IMethodSymbol).Cast<IMethodSymbol>();
                    foreach(var m in methods)
                    {
                        if (m.IsExtensionMethod)
                        {
                            if (await m.AnyRefMatch(
                                platformSolution, 
                                r =>
                                    usedTypes.Contains(r, comparer) || specialTypes.Contains(r, comparer)
                                                                    || !projects.Contains(r.GetDefineProject(solution))))
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
            }
            
            notUsedTypes.ExceptWith(specialTypes);

            _logger.WriteLine($"analysis types used by not used type at {DateTime.Now}");
            /// 第3轮分析：被notUsed引用的类型
            foreach(var type in usedTypes)
            {
                if (!typeRefs.TryGetValue(type, out var refs))
                {
                    continue;
                }
                
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
            _logger.WriteLine($"analysis finish at {DateTime.Now}");
            
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
            
            solution = await solution.RemoveSymbolsDefinitionAsync(notUsedTypes.Concat(usedBySelfTypes)
                .Concat(usedByNotUsedTypeTypes));
            return solution;
        }

        public static async Task<bool> AnyRefMatch(this ISymbol symbol, Solution solution, Func<INamedTypeSymbol, bool> checker)
        {
            var refs = (await SymbolFinder.FindCallersAsync(symbol, solution)).Select(r => r.CallingSymbol.GetDefineType()).ToArray();
            if (refs.Any(checker))
            {
                return true;
            }

            return false;
        }

        private static async Task<Solution> CheckFilesAndRemove(Solution solution, IEnumerable<Project> projects, IEnumerable<(string[] add, string[] rm)> platforms, Func<Document, bool> documentFilter)
        {
            var nsComparer = new NamespaceSymbolEqualityComparer();
            var docComparer = new DocumentEqualityComparer();
            HashSet<INamespaceSymbol> retainNss = new HashSet<INamespaceSymbol>(nsComparer);
            HashSet<INamespaceSymbol> rmNss = new HashSet<INamespaceSymbol>(nsComparer);
            HashSet<Document> rmDocs = new HashSet<Document>(docComparer);
            HashSet<Document> retainDocs = new HashSet<Document>(docComparer);

            //find docs without type definition under all platforms
            foreach (var platform in platforms)
            {
                foreach (var project in projects)
                {
                    var np = project.WithParseOptions(project.OptionWithChangeSymbols(platform.add, platform.rm));
                    foreach (var document in np.Documents)
                    {
                        if (document.Name == "AndroidImpl") Debugger.Break();
                        if (!documentFilter(document)) continue;
                        if (retainDocs.Contains(document)) continue;
                
                        var root = await document.GetSyntaxRootAsync();
                        
                        if (!root.ContainsTypeDeclaration())
                        {
                            rmDocs.Add(document);
                            var nsDefines = document.GetNamespaceDefinition(root);
                            await foreach(var define in nsDefines) rmNss.Add(define);
                        }
                        else
                        {
                            retainDocs.Add(document);
                            var nsDefines = document.GetNamespaceDefinition(root);
                            await foreach(var define in nsDefines) retainNss.Add(define);
                        }
                    }
                }
            }
            
            rmDocs.ExceptWith(retainDocs);
            rmNss.ExceptWith(retainNss);

            foreach (var platform in platforms)
            {
                var tmpSolution = solution.WithChangeSymbols(platform.add, platform.rm);
                
                foreach (var ns in rmNss)
                {
                    var refs = await SymbolFinder.FindReferencesAsync(ns, tmpSolution);
                    foreach (var @ref in refs)
                    {
                        foreach (var loc in @ref.Locations)
                        {
                            if (rmDocs.Contains(loc.Document))
                            {
                                continue;
                            }
                            _logger.WriteLine($"remove using of {ns.Name} from {loc.Document.Name}:{loc.Location.GetLineSpan()}");
                            var root = loc.Location.SourceTree.GetRoot();
                            var node = root.FindNode(loc.Location.SourceSpan);
                            int level = 4;
                            while (level-- > 0 && (node != null))
                            {
                                if (node is UsingDirectiveSyntax)
                                {
                                    break;
                                }
                                node = node.Parent;
                            }

                            if (node == null)
                            {
                                continue;
                            }
                            root = root.RemoveNode(node, SyntaxRemoveOptions.KeepUnbalancedDirectives);
                            solution = solution.WithDocumentSyntaxRoot(loc.Document.Id, root);
                        }
                    }
                }
            }

            foreach (var doc in rmDocs)
            {
                _logger.WriteLine($"remove document: {doc.FilePath}");
                solution = solution.RemoveDocument(doc.Id);
            }

            return solution;
        }

        private static bool InputContinue()
        {
            var input = Console.ReadLine();
            return input == "Y" || input == "y";
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

    class TypeSymbolEqualityComparer : IEqualityComparer<ISymbol>
    {
        public bool Equals(ISymbol x, ISymbol y)
        {
            if (ReferenceEquals(x, y)) return true;
            if (x == null || y == null) return false;
            return x.Name == y.Name && x.ContainingNamespace?.Name == y.ContainingNamespace?.Name;
        }

        public int GetHashCode(ISymbol obj)
        {
            if (obj is INamedTypeSymbol typeSymbol)
            {
                return $"{typeSymbol.ContainingNamespace}.{typeSymbol.Name}".GetHashCode();
            }

            return obj.GetHashCode();
        }
    }

    class DocumentEqualityComparer : IEqualityComparer<Document>
    {
        public bool Equals(Document x, Document y)
        {
            if (ReferenceEquals(x, y)) return true;
            if (x == null || y == null) return false;
            return x.FilePath == y.FilePath;
        }

        public int GetHashCode(Document obj)
        {
            return obj.FilePath?.GetHashCode() ?? obj.Id.GetHashCode();
        }
    }

    class NamespaceSymbolEqualityComparer : IEqualityComparer<INamespaceSymbol>
    {
        public bool Equals(INamespaceSymbol x, INamespaceSymbol y)
        {
            if (ReferenceEquals(x, y)) return true;
            if (x == null || y == null) return false;
            return x.Name == y.Name;
        }

        public int GetHashCode(INamespaceSymbol obj)
        {
            return obj.Name.GetHashCode();
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

        public static async IAsyncEnumerable<INamespaceSymbol> GetNamespaceDefinition(this Document document, SyntaxNode syntaxRoot)
        {
            SemanticModel m = await document.GetSemanticModelAsync();
            var nodes = syntaxRoot.DescendantNodes().ToArray();
            foreach (var c in nodes.OfType<NamespaceDeclarationSyntax>())
            {
                yield return m.GetDeclaredSymbol(c);
            }
        }
        
        public static async IAsyncEnumerable<INamedTypeSymbol> GetAllNamedType(this Project project, Func<Document, bool> documentFilter)
        {
            foreach (Document d in project.Documents)
            {
                if (!documentFilter(d)) continue;
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
                var newRoot = root.RemoveNode(node, SyntaxRemoveOptions.KeepUnbalancedDirectives);
                if (!newRoot.DescendantNodes().Any(n => n is TypeDeclarationSyntax))
                {
                    Debugger.Break();
                }
                solution = solution.WithDocumentSyntaxRoot(docId, newRoot);
            }

            return solution;
        }

        public static async Task<Solution> RemoveSymbolsDefinitionAsync(this Solution solution, IEnumerable<ISymbol> s)
        {
            var symbolGroups = s.SelectMany(s => s.DeclaringSyntaxReferences)
                .Select(d => d.GetSyntax())
                .GroupBy(node => solution.GetDocumentId(node.SyntaxTree)).ToArray();
            foreach (var symbolsPerDoc in symbolGroups)
            {
                var root = await symbolsPerDoc.First().SyntaxTree.GetRootAsync();
                root = root.RemoveNodes(symbolsPerDoc, SyntaxRemoveOptions.KeepUnbalancedDirectives);
                {
                    solution = solution.WithDocumentSyntaxRoot(symbolsPerDoc.Key, root);
                }
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
                if (baseTypeNames.Contains($"{symbol.BaseType.ContainingNamespace}.{symbol.BaseType.Name}")) return true;
                symbol = symbol.BaseType;
            }

            
            return false;
        }

        public static Project GetDefineProject(this ISymbol symbol, Solution solution)
        {
            return solution.GetProject(symbol.ContainingAssembly);
        }

        public static bool ContainsTypeDeclaration(this SyntaxNode documentRoot)
        {
            return documentRoot.DescendantNodes().Any(n => n is TypeDeclarationSyntax || n is EnumDeclarationSyntax);
        }

        public static Solution WithChangeSymbols(this Solution solution, IEnumerable<string> addSymbols, IEnumerable<string> removeSymbols)
        {
            foreach (var project in solution.Projects)
            {
                solution = solution.WithProjectParseOptions(project.Id, project.OptionWithChangeSymbols(addSymbols, removeSymbols));
            }

            return solution;
        }
        
        public static CSharpParseOptions OptionWithChangeSymbols(this Project project, IEnumerable<string> addSymbols, IEnumerable<string> removeSymbols)
        {
            var rawOption = project.ParseOptions as CSharpParseOptions ?? new CSharpParseOptions(
                LanguageVersion.Latest);
            
            var combinedSymbols =
                addSymbols.Concat(rawOption.PreprocessorSymbolNames.Except(removeSymbols)).Distinct();
            
            return rawOption.WithPreprocessorSymbols(combinedSymbols);
        }
    }
}
