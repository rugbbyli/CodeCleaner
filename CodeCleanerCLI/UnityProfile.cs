using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace CodeCleanerCLI
{
    public class UnityProfile
    {
        private static readonly string[] unityWinSymbols = { "UNITY_STANDALONE", "UNITY_STANDALONE_WIN" };
        private static readonly string[] unityAndroidSymbols = { "UNITY_ANDROID" };
        private static readonly string[] unityiOSSymbols = { "UNITY_IPHONE", "UNITY_IOS" };
        private static readonly string[] unityMacSymbols = { "UNITY_STANDALONE", "UNITY_STANDALONE_OSX" };
        private static readonly string[] unityEditorSymbols = { "UNITY_EDITOR", "UNITY_EDITOR_64", "UNITY_EDITOR_WIN", "UNITY_EDITOR_OSX", "UNITY_EDITOR_LINUX" };

        public static readonly string[] ignorePaths =
        {
            $"Assets{Path.DirectorySeparatorChar}Components3rd{Path.DirectorySeparatorChar}",
            $"Assets{Path.DirectorySeparatorChar}Standard Assets{Path.DirectorySeparatorChar}",
        };

        public static IEnumerable<(string[] add, string[] rm)> EnumUnityPlatformSymbols()
        {
            return new[] {
                (unityAndroidSymbols, unityEditorSymbols.Concat(unityWinSymbols).Concat(unityiOSSymbols).Concat(unityMacSymbols).ToArray()), 
                (unityWinSymbols, unityEditorSymbols.Concat(unityAndroidSymbols).Concat(unityiOSSymbols).Concat(unityMacSymbols).ToArray()), 
                (unityiOSSymbols, unityEditorSymbols.Concat(unityWinSymbols).Concat(unityAndroidSymbols).Concat(unityMacSymbols).ToArray()), 
                (unityMacSymbols, unityEditorSymbols.Concat(unityWinSymbols).Concat(unityiOSSymbols).Concat(unityAndroidSymbols).ToArray()), 
                (unityEditorSymbols, new string[0])
            };
        }

        public static string[] IgnoreBaseTypes = {"UnityEngine.Object"};

        public static string SolutionPath = "/Volumes/hjdclient.3d/hjdclient.3d/hjdclient.3d.sln";

        public static string[] ProjectNames = {
            "Assembly-CSharp",
            //"Assembly-CSharp-firstpass"
        };

        public static Options BuildOption()
        {
            return new Options()
            {
                SolutionPath = SolutionPath,
                ProjectNames = ProjectNames,
                IgnorePaths = ignorePaths,
                IgnoreBaseTypes = IgnoreBaseTypes,
                _rawDefineList = EnumUnityPlatformSymbols().Select(FormatSymbolDefine),
            };
        }

        public static string FormatSymbolDefine((string[] add, string[] rm) define)
        {
            StringBuilder sb = new StringBuilder();
            if (define.add.Length > 0)
            {
                sb.Append('+');
                sb.AppendJoin('+', define.add);
            }
            if (define.rm.Length > 0)
            {
                sb.Append('-');
                sb.AppendJoin('-', define.rm);
            }

            return sb.ToString();
        }
    }
}