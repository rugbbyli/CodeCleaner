using System;
using System.Collections.Generic;
using System.Linq;
using CommandLine;

namespace CodeCleanerCLI
{
    public class Options
    {
        [Option("solution", Required = true, HelpText = "Input .sln file path.")]
        public string SolutionPath { get; set; }
        

        [Option('a', "action", Required = true)]
        public ActionType Action { get; set; }
        
        [Option("ignore-path", Required = false)]
        public IEnumerable<string> IgnorePaths { get; set; }
        [Option("ignore-type-base", Required = false)]
        public IEnumerable<string> IgnoreBaseTypes { get; set; }

        [Option("project", Required = false, HelpText = "Input project name to only work on specific projects.")]
        public IEnumerable<string> ProjectNames { get; set; }
        [Option("define", Required = false, HelpText = "define preprocessor symbols, format: +A+B-C-D, means add symbol A and B, remove symbol C and D.")]
        public IEnumerable<string> _rawDefineList { get; set; }
        public IEnumerable<Defines> DefineList
        {
            get
            {
                return _rawDefineList.Select(i =>
                {
                    Defines defines = default;
                    var index = i.IndexOf('-');
                    if (index < 0)
                    {
                        defines.Add = i.Split('+', StringSplitOptions.RemoveEmptyEntries);
                        defines.Remove = Array.Empty<string>();
                    }
                    else if (index == 0)
                    {
                        defines.Add = Array.Empty<string>();
                        defines.Remove = i.Split('-', StringSplitOptions.RemoveEmptyEntries);
                    }
                    else
                    {
                        defines.Add = i.Remove(index).Split('+', StringSplitOptions.RemoveEmptyEntries);
                        defines.Remove = i.Substring(index).Split('-', StringSplitOptions.RemoveEmptyEntries);
                    }

                    return defines;
                });
            }
        }
        
        [Option('d', "dry-run", Required = false, HelpText = "set true to not actually save changes.")]
        public bool DryRun { get; set; }
        
        public struct Defines
        {
            public string[] Add;
            public string[] Remove;
        }
        
        public enum ActionType
        {
            RemoveUnusedType,
            RemoveUnusedFile,
        }
    }
}