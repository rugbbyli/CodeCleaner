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
using CommandLine;
using CommandLine.Text;

namespace CodeCleanerCLI
{
    static class Program
    {
        private static ILogger _logger;
        
        static async Task<int> Main(string[] args)
        {
            var parserResult = Parser.Default.ParseArguments<Options>(args);

            if (parserResult is Parsed<Options> opt)
            {
                var options = opt.Value;
#if DEBUG
                options.SolutionPath = "/Volumes/hjdclient.3d/hjdclient.3d/hjdclient.3d.sln";
                // _logger = new FileLogger(Path.Combine(Environment.CurrentDirectory, $"{options.Action.ToString()}-{DateTime.Now}.log")){WriteToConsole = true};
                _logger = new ConsoleLogger();
#else         
                _logger = new ConsoleLogger();
#endif
                return await Work(options);
            }
            else
            {
                _logger = new ConsoleLogger();
                parserResult.WithNotParsed(errs => DisplayHelp(parserResult, errs));
                return 0;
            }

        }
        
        static void DisplayHelp<T>(ParserResult<T> result, IEnumerable<Error> errs)
        {
            HelpText helpText = null;
            helpText = HelpText.AutoBuild(result, h =>
            {
                h.AddEnumValuesToHelpText = true;
                return HelpText.DefaultParsingErrorsHandler(result, h);
            }, e => e);
            _logger.WriteLine(helpText);
        }
        
        static async Task<int> Work(Options options)
        {
            int exitCode = 0;
            _logger.WriteLine($"solution: {options.SolutionPath}");

            MSBuildLocator.RegisterDefaults();
            using MSBuildWorkspace workspace = MSBuildWorkspace.Create();
            // workspace.WorkspaceFailed += Workspace_WorkspaceFailed;

            List<Project> projects = new List<Project>();
            Solution solution = await workspace.OpenSolutionAsync(options.SolutionPath);
            if (options.IncludeProjects.Any())
            {
                projects.AddRange(options.IncludeProjects.Select(name => solution.Projects.FirstOrDefault(p => p.Name == name)).Where(p => p != null).Distinct());
            }
            else if (options.ExcludeProjects.Any())
            {
                projects.AddRange(solution.Projects.Where(p => !options.ExcludeProjects.Contains(p.Name)).Distinct());
            }
            else
            {
                projects.AddRange(solution.Projects);
            }
            
            var solutionFolder = Path.GetDirectoryName(Path.GetFullPath(options.SolutionPath));
            bool FilterDoc(Document doc)
            {
                return doc.FilePath!.StartsWith(solutionFolder!) && !options.IgnorePaths.Any(path => doc.FilePath!.Contains(path));
            }

            var allPlatformSymbols = options.DefineList.Select(d => (d.Add, d.Remove)).ToArray();

            if(options.Action == Options.ActionType.RemoveUnusedType)
            {
                _logger.WriteLine($"==================remove not used type=====================");
                solution = await RemoveUnUsedTypes(solution, projects, allPlatformSymbols, FilterDoc, options.IgnoreBaseTypes);
            }
            else if(options.Action == Options.ActionType.RemoveUnusedFile)
            {
                _logger.WriteLine($"==================remove not used files=====================");
                solution = await CheckFilesAndRemove(solution, projects, allPlatformSymbols, FilterDoc);
            }
            else if (options.Action == Options.ActionType.CheckBlockWords)
            {
                _logger.WriteLine($"=================check block words=================");
                var fired = await CheckBlockWords.Run(solution, projects, allPlatformSymbols, options.BlockWordDimensions, _logger);
                if (fired) exitCode = 1;
            }
            if (options.DryRun == false)
            {
                var ret = workspace.TryApplyChanges(solution);
                _logger.WriteLine($"finish and save changes, result: {ret}");
            }
            (_logger as IDisposable)?.Dispose();

            return exitCode;
        }

        private static async Task<Solution> RemoveUnUsedTypes(Solution solution, IEnumerable<Project> projects, IEnumerable<(string[] add, string[] rm)> platforms, Func<Document, bool> documentFilter, IEnumerable<string> ignoreBaseTypes)
        {
            var comparer = new TypeSymbolEqualityComparer();
            
            HashSet<ISymbol> usedTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> usedBySelfTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> notUsedTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> usedByNotUsedTypeTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> specialTypes = new HashSet<ISymbol>(comparer);

            Dictionary<ISymbol, List<ReferencedSymbol>> typeRefs = new Dictionary<ISymbol, List<ReferencedSymbol>>(comparer);

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
                        if (type.IsInheritFromAny(ignoreBaseTypes))
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
                        if(result.Type.IsGenericType && result.Type.Name.Contains("RoomPlayersModel")) Debugger.Break();
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
                    if (type.EnumUnderlyingType != null) continue;
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
                foreach (var c in nodes.OfType<BaseTypeDeclarationSyntax>())
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

        public static bool IsInheritFromAny(this INamedTypeSymbol symbol, IEnumerable<string> baseTypeNames)
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
