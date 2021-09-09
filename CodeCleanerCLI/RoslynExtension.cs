using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;

namespace CodeCleanerCLI
{
    public static class RoslynExtension
    {
        public static IEnumerable<Project> ApplyProjectSymbols(this Solution solution,
            IEnumerable<Project> projects,
            IEnumerable<(string[] add, string[] rm)> symbolChanges)
        {
            return symbolChanges
                .Select(p => solution.WithChangeSymbols(p.add, p.rm))
                .SelectMany(ps => projects.Select(p => ps.FindProjectByName(p.Name)));
        }

        public static Project FindProjectByName(this Solution solution, string projectName)
        {
            return solution.Projects.FirstOrDefault(p => p.Name == projectName);
        }
        
        public static Solution WithChangeSymbols(this Solution solution, IEnumerable<string> addSymbols, IEnumerable<string> removeSymbols)
        {
            foreach (var project in solution.Projects)
            {
                solution = solution.WithProjectParseOptions(project.Id, project.OptionWithChangeSymbols(addSymbols, removeSymbols));
            }

            return solution;
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
            
            var parent = symbol.BaseType;
            while (parent != null)
            {
                if (baseTypeNames.Contains($"{parent.ContainingNamespace}.{parent.Name}")) return true;
                parent = parent.BaseType;
            }

            return symbol.AllInterfaces.Any(i => baseTypeNames.Contains($"{i.ContainingNamespace}.{i.Name}"));

            // return false;
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
}