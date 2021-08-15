using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFacts;

namespace CodeCleanerCLI
{
    static class CheckKeywords
    {
        private static int visitCount, skipCount;
        private static ILogger _logger;
        
        public static async Task<bool> Run(Solution solution, IEnumerable<Project> projects,
            IEnumerable<(string[] add, string[] rm)> symbolGroups, IEnumerable<string> keywords, ILogger logger)
        {
            logger.WriteLine($"begin check keywords at {DateTime.Now}");
            var cacheKeywords = keywords as string[] ?? keywords.ToArray();
            logger.WriteLine($"keywords: {string.Join(",", cacheKeywords)}");

            _logger = logger;
            
            visitCount = 0;
            skipCount = 0;
            
            var projectInstances = solution.ApplyProjectSymbols(projects, symbolGroups);

            var scanTasks = projectInstances.Select(p => ScanProjectAsync(p, cacheKeywords).ToListAsync().AsTask());
            var fired = (await Task.WhenAll(scanTasks)).SelectMany(t => t).ToList();

            logger.WriteLine($"visit {visitCount} tokens, skip {skipCount}({skipCount*100f/visitCount:F1}%), found {fired.Count}.");
            var pathBase = Path.GetDirectoryName(solution.FilePath);
            foreach (var f in fired)
            {
                var lineSpan = f.GetLocation().GetLineSpan();
                logger.WriteLine($"  --> {f.Text} in {lineSpan.Path.Replace(pathBase!, String.Empty)}[line {lineSpan.StartLinePosition.Line+1}])");
            }
            logger.WriteLine($"finish check keywords at {DateTime.Now}");

            return fired.Count > 0;
        }

        private static async IAsyncEnumerable<SyntaxToken> ScanProjectAsync(Project project,
            IEnumerable<string> keywords)
        {
            // _logger.WriteLine($"scaning {project.Name}");
            foreach (Document d in project.Documents)
            {
                await foreach (var token in ScanDocumentAsync(d, keywords))
                {
                    yield return token;
                }
            }
        }

        private static async IAsyncEnumerable<SyntaxToken> ScanDocumentAsync(Document document, IEnumerable<string> keywords)
        {
            // logger.WriteLine($"  {d.Name}");
            // SemanticModel m = await d.GetSemanticModelAsync();
            var syntaxRoot = await document.GetSyntaxRootAsync();

            // foreach (var node in syntaxRoot.DescendantNodes())
            // {
            //     if (FireNode(node, keywords, ref checkingToken))
            //     {
            //         var ts = node.Span;
            //         var loc = node.GetLocation();
            //         var tree = node.SyntaxTree;
            //         
            //         var option = tree.Options as CSharpParseOptions ?? new CSharpParseOptions();
            //         option = option.WithPreprocessorSymbols("UNITY_IOS", "UNITY_IPHONE");
            //         var newTree = tree.WithRootAndOptions(syntaxRoot, option);
            //         
            //         var compilation = m.Compilation.ReplaceSyntaxTree(tree, newTree);
            //         var diagnostics = newTree.GetDiagnostics();
            //         try
            //         {
            //             var newNode = newTree.GetRoot().FindNode(ts);
            //             if (FireNode(newNode, keywords, ref checkingToken))
            //             {
            //                 fired.Add(checkingToken);
            //             }
            //         }
            //         catch (Exception e)
            //         {
            //             
            //         }
            //     }
            // }

            foreach (var token in syntaxRoot.DescendantTokens())
            {
                visitCount++;
                var kind = token.Kind();
                if (IsReservedKeyword(kind) 
                    || IsContextualKeyword(kind) 
                    || IsQueryContextualKeyword(kind)
                    || IsPreprocessorKeyword(kind)
                    || IsTypeParameterVarianceKeyword(kind)
                    || IsPunctuationOrKeyword(kind)
                    || IsAccessorDeclarationKeyword(kind)
                    )
                {
                    skipCount++;
                    continue;
                }
            
                if (!token.Text.ContainsAny(keywords)) continue;

                yield return token;
            }
        }
        
        private static bool FireNode(SyntaxNode node, IEnumerable<string> keywords, ref SyntaxToken target)
        {
            var parsed =
                    TryParse<BaseTypeDeclarationSyntax>(node, s => s.Identifier, ref target)
                    || TryParse<DelegateDeclarationSyntax>(node, s => s.Identifier, ref target)
                    || TryParse<EnumMemberDeclarationSyntax>(node, s => s.Identifier, ref target)
                    || TryParse<MethodDeclarationSyntax>(node, s => s.Identifier, ref target)
                    || TryParse<VariableDeclaratorSyntax>(node, s => s.Identifier, ref target)
                    || TryParse<ParameterSyntax>(node, s => s.Identifier, ref target)
                    || TryParse<NameEqualsSyntax>(node, s => s.Name.Identifier, ref target)
                    || TryParse<LiteralExpressionSyntax>(node, s => s.Token, ref target)
                    || TryParse<InterpolatedStringTextSyntax>(node, s => s.TextToken, ref target)
                ;
            
            if (!parsed) return false;

            SyntaxToken token = target;
            if (token.Text.ContainsAny(keywords))
            {
                return true;
            }

            return false;
        }

        private static bool TryParse<T>(SyntaxNode node, Func<T, SyntaxToken> parser, ref SyntaxToken token)
        {
            if (node is T typeDeclarationSyntax)
            {
                token = parser(typeDeclarationSyntax);
                return true;
            }

            return false;
        }

        private static bool IsConditionalCompiledWithout(SyntaxToken token, string conditionStr)
        {
            var node = token.Parent;
            if (!node.ContainsDirectives) return false;
            var directive = node.GetFirstDirective();
            if (directive == null) return false;
            // #if or #elif
            if (directive is ConditionalDirectiveTriviaSyntax conditionalDirectiveTriviaSyntax)
            {
                // a or b
                // a and b
                // !a
                var condition = conditionalDirectiveTriviaSyntax.Condition;
                ConditionalExpression e;
            }
            // #else
            else if (directive is ElseDirectiveTriviaSyntax elseDirectiveTriviaSyntax)
            {
                
            }

            return false;
        }
        
        private class EnsureNoConditionalCompileSymbolWalker : CSharpSyntaxWalker
        {
            private readonly string _symbol;
            public bool Pass { get; private set; }

            public EnsureNoConditionalCompileSymbolWalker(string symbol)
            {
                _symbol = symbol;
            }
            
            public override void VisitBinaryExpression(BinaryExpressionSyntax node)
            {
                if(node.Left is BinaryExpressionSyntax left) VisitBinaryExpression(left);
                if(node.Right is BinaryExpressionSyntax right) VisitBinaryExpression(right);
                base.VisitBinaryExpression(node);
            }

            public override void VisitIdentifierName(IdentifierNameSyntax node)
            {
                if (node.Identifier.Text == _symbol) Pass = false;
                base.VisitIdentifierName(node);
            }
        }

        private static bool ContainsAny(this string str, IEnumerable<string> look)
        {
            return look.Any(i => str.Contains(i, StringComparison.CurrentCultureIgnoreCase));
        }
    }
}