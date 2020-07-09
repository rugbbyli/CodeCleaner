using System;
using System.IO;

namespace CodeCleanerCLI
{
    public class FileLogger : IDisposable
    {
        private TextWriter _textWriter;
        public bool WriteToConsole { get; set; }
        public FileLogger(string path)
        {
            Directory.CreateDirectory(Path.GetDirectoryName(path));
            _textWriter = new StreamWriter(File.OpenWrite(path));
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
            _textWriter?.Dispose();
        }
    }
}