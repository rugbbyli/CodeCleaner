using System;
using System.Collections.Generic;
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
        public static async Task<IEnumerable<string>> Run(Solution solution, IEnumerable<Project> projects,
            IEnumerable<(string[] add, string[] rm)> symbolGroups, IEnumerable<string> keywords, ILogger logger)
        {
            List<SyntaxToken> fired = new List<SyntaxToken>();
            int visitCount = 0;
            int skipCount = 0;
            var projectInstances = solution.ApplyProjectSymbols(projects, symbolGroups);
            SyntaxToken checkingToken = default;
            foreach (var project in projectInstances)
            {
                foreach (Document d in project.Documents)
                {
                    SemanticModel m = await d.GetSemanticModelAsync();
                    var syntaxRoot = await d.GetSyntaxRootAsync();

                    foreach (var node in syntaxRoot.DescendantNodes())
                    {
                        if (FireNode(node, keywords, ref checkingToken))
                        {
                            var ts = node.Span;
                            var loc = node.GetLocation();
                            var tree = node.SyntaxTree;
                            
                            var option = tree.Options as CSharpParseOptions ?? new CSharpParseOptions();
                            option = option.WithPreprocessorSymbols("UNITY_IOS", "UNITY_IPHONE");
                            var newTree = tree.WithRootAndOptions(syntaxRoot, option);
                            
                            var compilation = m.Compilation.ReplaceSyntaxTree(tree, newTree);
                            var diagnostics = newTree.GetDiagnostics();
                            try
                            {
                                var newNode = newTree.GetRoot().FindNode(ts);
                                if (FireNode(newNode, keywords, ref checkingToken))
                                {
                                    fired.Add(checkingToken);
                                }
                            }
                            catch (Exception e)
                            {
                                
                            }
                        }
                    }

                    // foreach (var token in syntaxRoot.DescendantTokens())
                    // {
                    //     visitCount++;
                    //     var kind = token.Kind();
                    //     if (IsReservedKeyword(kind) 
                    //         || IsContextualKeyword(kind) 
                    //         || IsQueryContextualKeyword(kind)
                    //         || IsPreprocessorKeyword(kind)
                    //         || IsTypeParameterVarianceKeyword(kind)
                    //         || IsPunctuationOrKeyword(kind)
                    //         || IsAccessorDeclarationKeyword(kind)
                    //         )
                    //     {
                    //         skipCount++;
                    //         continue;
                    //     }
                    //
                    //     if (!token.Text.ContainsAny(keywords)) continue;
                    //
                    //     /// 预编译：
                    //     /// 被非ios预编译，pass
                    //     /// 1. 不被预编译，error
                    //     /// 2. 被ios预编译，error
                    //
                    //     if (IsConditionalCompiledWithout(token, "UNITY_IOS")) continue;
                    //     {
                    //         fired.Add(token);
                    //     }
                    // }
                }
            }
            
            logger.WriteLine($"visit {visitCount} tokens, skip {skipCount}({skipCount*100f/visitCount:F1}%), found {fired.Count}.");

            return fired.Select(f => $"[{f.Kind()}]{f.Text} in {f.GetLocation()})");
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