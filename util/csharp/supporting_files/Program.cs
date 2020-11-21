using System;

namespace AOC
{
    class Program
    {
        private static void PartOne() 
        {
            Console.WriteLine("Solving part 1");
        }

        private static void PartTwo()
        {
            Console.WriteLine("Solving part 2");
        }

        static int Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Must provide part argument, `1` or `2`.");
                return 1;
            }

            switch(args[0])
            {
                case "1":
                    Program.PartOne();
                    break;
                case "2":
                    Program.PartTwo();
                    break;
                default:
                    Console.WriteLine("Only parts 1 and 2 exist...");
                    return 1;
            }

            return 0;
        }
    }
}
