using System;
using System.IO;

namespace CodeCleanerCLI
{
    interface ILogger
    {
        void WriteLine(string content);
    }
    public class FileLogger : IDisposable, ILogger
    {
        private TextWriter _textWriter;
        public bool WriteToConsole { get; set; }
        public FileLogger(string path)
        {
            Directory.CreateDirectory(Path.GetDirectoryName(path));
            _textWriter = File.CreateText(path);
        }

        public void WriteLine(string content)
        {
            _textWriter.WriteLine(content);
            if(WriteToConsole)
                Console.WriteLine(content);
        }

        public void Dispose()
        {
            _textWriter.Flush();
            _textWriter.Close();
            _textWriter.Dispose();
        }
    }

    public class ConsoleLogger : ILogger
    {
        public void WriteLine(string content) => Console.WriteLine(content);
    }
}