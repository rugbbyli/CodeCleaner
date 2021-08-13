using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace CodeCleanerCLI
{
    public static class SolutionHelper
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
    }
}