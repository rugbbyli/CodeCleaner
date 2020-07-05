using System;

namespace ConsoleApp
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
        }
    }

    class NotUsedFixedType : NotUsedFixedType2 {
        int i = 1;
        int j = 2;
    }

    class NotUsedFixedType2{}

    class NotUsedBase { }
    class NotUsedInherit : NotUsedBase { }

    class NotUsedClass
    {
        private int Prop = 0;
        public int Method() 
        {
            int i = 1;
            return Prop + i; 
        }

        class NotUsedSubClass { }
    }

    class NotUsed2
    {
        public NotUsed2 notUsed2;
    }

    struct NotUsedStruct
    {

    }

    class UsedA
    {
        public UsedB B;
    }

    class UsedB
    {
        public UsedA A;
        public int B => 2.MinusOne();
    }

    static class NotUsedExt
    {
        public static int AddOne(this int i) => i + 1;
    }
    static class UsedExt
    {
        public static int MinusOne(this int i) => i - 1;
    }
}
