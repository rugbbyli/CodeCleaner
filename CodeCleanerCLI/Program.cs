using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.MSBuild;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using CommandLine;
using CommandLine.Text;
using static CodeCleanerCLI.CodeCleaner;

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
                solution = await RemoveUnUsedTypes(solution, projects, allPlatformSymbols, FilterDoc, options.IgnoreBaseTypes.ToArray(), _logger);
            }
            else if(options.Action == Options.ActionType.RemoveUnusedFile)
            {
                _logger.WriteLine($"==================remove not used files=====================");
                solution = await CheckFilesAndRemove(solution, projects, allPlatformSymbols, FilterDoc, _logger);
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


        private static void Workspace_WorkspaceFailed(object sender, WorkspaceDiagnosticEventArgs e)
        {
            _logger.WriteLine($"[error]{e.Diagnostic.Message}");
        }
    }
}
