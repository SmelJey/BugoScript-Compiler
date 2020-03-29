using System;
using static Compiler.Compiler;

namespace Compiler {
    public class Tester {
        private static long testField = 5;
        public static long testField2 = 8;
        public static long x = 2;
        public static long y = 4;
        public static long z = 6;

        public static long MulAndSum(long x, long y, long z) {
            return x * y + z;
        }

        public static long Mul(long a, long b) {
            return a * b;
        }

        public static long Uno() {
            return 1;
        }

        public static void Print(long val) {
            Console.WriteLine($"Print from program: {val}");
        }

        public static bool TestAssert(bool exp) {
            if (!exp) {
                throw new Exception("Test failed");
            }

            return exp;
        }

        public static bool ThrowAssert<T, U>(Func<T, U> f, T arg) {
            try {
                f.Invoke(arg);
            } catch (Exception e) {
                Console.WriteLine($"Exception {e.Message} was thrown");
                return true;
            }

            throw new Exception("Invokation ended with no throw");
        }

        public static void CompileExpressionTest() {
            var result = CompileSingle("x * (1 - y) + z * z / 4");
            TestAssert(result.Invoke() == 3);

            var result2 = CompileSingle("x*4/2+(((x+y + z)))");
            TestAssert(result2.Invoke() == 16);

            var resultInt = CompileSingle("testField");
            TestAssert(resultInt.Invoke() == 5);

            var resultEmpty = CompileSingle("");
            resultEmpty.Invoke();

            var result3 = CompileSingle("y");
            TestAssert(result3.Invoke() == 4);

            var resultUnary = CompileSingle("-y + x - z + (-2) * (4)");
            TestAssert(resultUnary.Invoke() == -16);

            var resultUnary2 = CompileSingle("-y + x - z + (-2) * ((-(-4)))");
            TestAssert(resultUnary2.Invoke() == -16);

            var resultFunc = CompileSingle("-x + y*(-z) + (z + Uno())");
            TestAssert(resultFunc.Invoke() == -19);

            var resultArgFunc = CompileSingle("Mul(x, y)");
            TestAssert(resultArgFunc.Invoke() == 8);

            var resultArgsOrder = CompileSingle("MulAndSum(x, y, z)");
            TestAssert(resultArgsOrder() == 14);

            var resultUnaryOrder = CompileSingle("-12 / 3 - 6 / 2");
            TestAssert(resultUnaryOrder.Invoke() == -7);

            var resultArgsOrder2 = CompileSingle("MulAndSum(x, y, z) / 7 - 6 / (1 * 2)");
            TestAssert(resultArgsOrder2.Invoke() == -1);

            var resultFunc2 = CompileSingle("-x + y*(-z) + (z + Mul(x, y))");
            TestAssert(resultFunc2.Invoke() == -12);
            
            var resultJaggedFuncs = CompileSingle("MulAndSum(Mul(x, y), Mul(x, z) / 2 - Uno(), z) / 2 + 3");
            TestAssert(resultJaggedFuncs.Invoke() == 26);

            var resultVariable = CompileSingle("testField + 2");
            TestAssert(resultVariable.Invoke() == 7);

            //9
            var completeTest = CompileSingle("testField2 + testField * x / Mul(testField, 2) - (MulAndSum(x, y, 2) + Uno()) * Mul(testField, x) * 2 / Mul(y, 11)");
            TestAssert(completeTest.Invoke() == 4);
        }

        
        public static void CompilerTest() {
            var simpleTest = Compile("declare { a = 2;} program { return a; }");
            TestAssert(simpleTest.Invoke() == 2);

            var compilatorTestVars = Compile("" +
                "declare {" +
                "   a = 4;" +
                "   b = 2 + 1;" + // 3
                "   c = testField + testField2 / 2;" + // 9
                "   d = Mul(3, 2);" + // 6
                "}" +
                "program {" +
                "   Print(a);" +
                "   Print(b);" +
                "   Print(c);" +
                "   c = b + a;" + // 7
                "   Print(c);" +
                "   Print(d);" +
                "   d = Mul(b, c);" + // 21
                "   Print(d);" +
                "   return d;" +
                "}");
            TestAssert(compilatorTestVars.Invoke() == 21);

            var codeAfterReturnTest = Compile("program{return 1; Print(2); return 2;}");
            TestAssert(codeAfterReturnTest.Invoke() == 1);

            var simpleIfTest = Compile("program {" +
                "   if (testField - testField > 0) {" +
                "       Print(testField);" +
                "   } else {" +
                "       Print(testField2);" +
                "   }" +
                "   return 1;" +
                "}");
            TestAssert(simpleIfTest.Invoke() == 1);

            var simpleReturnIfTest = Compile("program {" +
                "   if (testField2 - testField > 0 ) {" +
                "       return 1;" +
                "   } else {" +
                "       Print(2);" +
                "       return 2;" +
                "   }" +
                "}");
            TestAssert(simpleReturnIfTest.Invoke() == 1);

            var compilatorTestIf = Compile("program{if(testField-testField2 < 0){ Print(testField);}else{Print(testField2);}" +
                "if(testField-testField > 0){return 1;}else{return 2;}}");
            TestAssert(compilatorTestIf.Invoke() == 2);

            var multipleReturnTest = Compile(
                "program{" +
                "   if (false) {" +
                "       Print(1);" +
                "       return 1;" +
                "   } else { if (false) {" +
                "       Print(2);" +
                "       return 2;" +
                "   } else{ if (true) {" +
                "       Print(3);" +
                "       return 3;" +
                "   } else { return 4;}}}" +
                "" +
                "   if (true) {" +
                "       Print(11);" +
                "   } else {" +
                "       Print(12);" +
                "       return 12;" +
                "   }" +
                "}");
            TestAssert(multipleReturnTest.Invoke() == 3);

            var multipleReturnTest2 = Compile(
                "program{" +
                "   if (false) {" +
                "       Print(1);" +
                "       return 1;" +
                "   } else { if (false) {" +
                "       Print(2);" +
                "       if (false) {" +
                "           return 2;" +
                "       } else {" +
                "           return 3;" +
                "       }" +
                "   } else{ if (true) {" +
                "       Print(3);" +
                "       return 3;" +
                "   } else { return 4;}}}" +
                "}");
            TestAssert(multipleReturnTest2.Invoke() == 3);

            var boolTest1 = Compile(
                "program {" +
                "   if 1 == 1 {" +
                "       return 1;" +
                "   } else {" +
                "       return 0;" +
                "   }" +
                "}");
            TestAssert(boolTest1.Invoke() == 1);

            var boolTest2 = Compile(
                "program {" +
                "   if ((1 == 1 && 2 == 2 + 1) || (1 != 2 && 5 <= 6 && 5 <= 5 && !( 5 < 4))) {" +
                "       return 1;" +
                "   } else {" +
                "       return 0;" +
                "   }" +
                "}");
            TestAssert(boolTest2.Invoke() == 1);

            var boolTest3 = Compile(
                "program {" +
                "   if ((true || false) && (true && (4 < 5 && 5 > 4) || false)) {" +
                "       return 1;" +
                "   } else {" +
                "       return 0;" +
                "   }" +
                "}");
            TestAssert(boolTest3.Invoke() == 1);
        }

        public static void ExceptionCompilerTest() {
            ThrowAssert(CompileSingle, "))testField(("); // Incorrect brackets
            ThrowAssert(CompileSingle, "Mul(2, 1"); // Incorrect brackets
            ThrowAssert(CompileSingle, "2 +"); // Incorrect expression
            ThrowAssert(CompileSingle, "+ 2 3"); // Incorrect expression
            ThrowAssert(CompileSingle, "2 (+) 3"); // Incorrect expression
            ThrowAssert(CompileSingle, "2 +/ 3"); // Incorrect expression

            ThrowAssert(Compile,
                "program{" +
                "   if (false) {" +
                "       Print(1);" +
                "       return 1;" +
                "   } else { if (false) {" +
                "       Print(2);" +
                "   } else{ if (true) {" +
                "       Print(3);" +
                "       return 3;" +
                "   }}}" +
                "}"
            ); // Not all branches return values

            ThrowAssert(Compile,
                "declare {" +
                "   a = 5;" +
                "   a = 4;" +
                "}" +
                "program{" +
                "   return a;" +
                "}"
            ); // Duplicate variables names

            ThrowAssert(Compile,
                "declare {" +
                "   true = 5;" +
                "   false = 4;" +
                "}" +
                "program{" +
                "   return a;" +
                "}"
            ); // Using of reserved names

            ThrowAssert(Compile,
                "declare {" +
                "   if (true) { a = 5; } else { a = 4; }" +
                "}" +
                "program{" +
                "   return a;" +
                "}"
            ); // If statement in declaration block

            ThrowAssert(Compile,
                "declare {" +
                "   Print(1);" +
                "}" +
                "program{" +
                "   return 1;" +
                "}"
            ); // Function call in declaration block

            ThrowAssert(Compile,
                "program{" +
                "   return testField;" +
                "}" +
                "declare {" +
                "   a = 4;" +
                "}"
            ); // Declaration block is not first

            ThrowAssert(Compile,
                "program{ return unknown; }"
            ); // Variable / Field does not exist

            ThrowAssert(Compile, "program { return 1;"); // Bracket is missing
            ThrowAssert(Compile, "return 1;"); // Using return outside of program
            ThrowAssert(Compile, "declare {a = 5;}"); // No program block
            ThrowAssert(Compile, "declare {a = 5;} declare {b = 5;} program { return a + b;}"); // Multiple declaration blocks
            ThrowAssert(Compile, "declare {a = 5;} program {a = 2; return 1;} program { return a;}"); // Multiple program blocks

            ThrowAssert(Compile,
                "program {" +
                "   Mul(2, 1);" +
                "   return Print(2);" +
                "}"); // Void function, while expecting long

            ThrowAssert(Compile,
                "program {" +
                "   if (Mul(2, 1)) { Print(2); }" +
                "   return 1;" +
                "}"); // Long in bool expression

            ThrowAssert(Compile,
                "program {" +
                "   if (2 && 3) {" +
                "       Print(1);" +
                "   }" +
                "   return 1;" +
                "}"); // Long instead of bool arg
            ThrowAssert(Compile,
                "program {" +
                "   if ((1 || false) && (true && (4 < 5 + true && 5 > 4) || false)) {" +
                "       return 1;" +
                "   } else {" +
                "       return 0;" +
                "   }" +
                "}"); // Long in bool operator and bool in long operator
        }

        public static void Main() {
            Tester.CompileExpressionTest();
            Tester.CompilerTest();
            Tester.ExceptionCompilerTest();
        }
    }
}
