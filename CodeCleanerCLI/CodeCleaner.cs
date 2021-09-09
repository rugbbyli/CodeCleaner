using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;

namespace CodeCleanerCLI
{
    internal static class CodeCleaner
    {
        public static async Task<Solution> RemoveUnUsedTypes(
            Solution solution, IEnumerable<Project> projects,
            IEnumerable<(string[] add, string[] rm)> platforms,
            Func<Document, bool> documentFilter,
            IEnumerable<string> ignoreBaseTypes,
            ILogger logger)
        {
            var comparer = new TypeSymbolEqualityComparer();

            HashSet<ISymbol> usedTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> usedBySelfTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> notUsedTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> usedByNotUsedTypeTypes = new HashSet<ISymbol>(comparer);
            HashSet<ISymbol> specialTypes = new HashSet<ISymbol>(comparer);

            Dictionary<ISymbol, List<ReferencedSymbol>> typeRefs =
                new Dictionary<ISymbol, List<ReferencedSymbol>>(comparer);

            var platformSolutions = platforms.Select(p => (p, solution.WithChangeSymbols(p.add, p.rm))).ToArray();

            logger.WriteLine($"analysis all type references at {DateTime.Now}");
            //获取全部类型
            foreach (var (platform, platformSolution) in platformSolutions)
            {
                foreach (var project in projects)
                {
                    logger.WriteLine(
                        $"analysis all type references from project {project.Name} with platform {string.Join('+', platform)} at {DateTime.Now}");
                    var np = platformSolution.Projects.First(p => p.Name == project.Name);
                    var taskGroup =
                        new Dictionary<INamedTypeSymbol,
                            Task<(INamedTypeSymbol Type, IEnumerable<ReferencedSymbol> Result)>>(comparer);
                    await foreach (var type in np.GetAllNamedType(documentFilter))
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
                        taskGroup.Add(type,
                            SymbolFinder.FindReferencesAsync(type, platformSolution)
                                .ContinueWith(t => (Type: type, t.Result)));
                    }

                    var results = await Task.WhenAll(taskGroup.Values);
                    foreach (var result in results)
                    {
                        if (result.Type.IsGenericType && result.Type.Name.Contains("RoomPlayersModel"))
                            Debugger.Break();
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

            logger.WriteLine($"analysis all type references finish, scan {typeRefs.Count} types.");

            logger.WriteLine($"analysis usage at {DateTime.Now}");
            //第一轮分析
            foreach (var (type, refs) in typeRefs)
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


            logger.WriteLine($"analysis extension methods at {DateTime.Now}");
            foreach (var (platform, platformSolution) in platformSolutions)
            {
                foreach (INamedTypeSymbol type in notUsedTypes)
                {
                    if (type.EnumUnderlyingType != null) continue;
                    //todo: not very safe, GetMembers may missing some members as it was created in one platform
                    var methods = type.GetMembers().Where(m => m is IMethodSymbol).Cast<IMethodSymbol>();
                    foreach (var m in methods)
                    {
                        if (m.IsExtensionMethod)
                        {
                            if (await m.AnyRefMatch(
                                platformSolution,
                                r =>
                                    usedTypes.Contains(r, comparer) || specialTypes.Contains(r, comparer)
                                                                    || !projects.Contains(
                                                                        r.GetDefineProject(solution))))
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

            logger.WriteLine($"analysis types used by not used type at {DateTime.Now}");
            /// 第3轮分析：被notUsed引用的类型
            foreach (var type in usedTypes)
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
                if (usages.All(usage => usage == UsageType.UsedBySelf || usage == UsageType.UsedBySomeoneWhoWasNotUsed))
                {
                    usedByNotUsedTypeTypes.Add(type);
                }

            }

            usedTypes.ExceptWith(usedByNotUsedTypeTypes);
            logger.WriteLine($"analysis finish at {DateTime.Now}");

            logger.WriteLine(
                $"[notUsedTotal]{notUsedTypes.Count + usedBySelfTypes.Count + usedByNotUsedTypeTypes.Count}");
            logger.WriteLine($"[usedBySelf]-------{usedBySelfTypes.Count}---------");
            foreach (var type in usedBySelfTypes)
            {
                logger.WriteLine($"[usedBySelf]{Format(type)}");
            }

            logger.WriteLine($"[usedByNotUsed]-------{usedByNotUsedTypeTypes.Count}---------");
            foreach (var type in usedByNotUsedTypeTypes)
            {
                logger.WriteLine($"[usedByNotUsed]{Format(type)}");
            }

            logger.WriteLine($"[notUsed]-------{notUsedTypes.Count}---------");
            foreach (var type in notUsedTypes)
            {
                logger.WriteLine($"[notUsed]{Format(type)}");
            }

            solution = await solution.RemoveSymbolsDefinitionAsync(notUsedTypes.Concat(usedBySelfTypes)
                .Concat(usedByNotUsedTypeTypes));
            return solution;
        }

        private static string Format(ISymbol symbol)
        {
            return $"{symbol.Name} in {symbol.Locations[0].GetLineSpan()}";
        }

        public static async Task<bool> AnyRefMatch(this ISymbol symbol, Solution solution,
            Func<INamedTypeSymbol, bool> checker)
        {
            var refs = (await SymbolFinder.FindCallersAsync(symbol, solution))
                .Select(r => r.CallingSymbol.GetDefineType()).ToArray();
            if (refs.Any(checker))
            {
                return true;
            }

            return false;
        }

        public static async Task<Solution> CheckFilesAndRemove(
            Solution solution, 
            IEnumerable<Project> projects,
            IEnumerable<(string[] add, string[] rm)> platforms, 
            Func<Document, bool> documentFilter,
            ILogger logger)
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
                            await foreach (var define in nsDefines) rmNss.Add(define);
                        }
                        else
                        {
                            retainDocs.Add(document);
                            var nsDefines = document.GetNamespaceDefinition(root);
                            await foreach (var define in nsDefines) retainNss.Add(define);
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

                            logger.WriteLine(
                                $"remove using of {ns.Name} from {loc.Document.Name}:{loc.Location.GetLineSpan()}");
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
                logger.WriteLine($"remove document: {doc.FilePath}");
                solution = solution.RemoveDocument(doc.Id);
            }

            return solution;
        }

    }

    enum UsageType
    {
        Used,
        UsedBySelf,
        UsedBySomeoneWhoWasNotUsed,
    }
}