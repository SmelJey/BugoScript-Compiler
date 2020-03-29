using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

namespace JetbrainsInternship {
    class Program {
        private static long testField = 5;
        public static long testField2 = 8;

        public static long MulAndSum(long x, long y, long z) {
            return x * y + z;
        }

        public static long Mul(long a, long b) {
            return a * b;
        }

        public static int Uno() {
            return 1;
        }

        delegate long CompileResult(long x, long y, long z);

        enum TokenType {
            Variable,
            Number,
            X,
            Y,
            Z,
            BracketsOpen,
            BracketsClose,
            BinaryOperator,
            UnaryOperator,
            Function,
            Comma,
        }

        /// <summary>
        /// State of RPN state machine
        /// </summary>
        enum RPNState {
            /// <summary>  Expression just started </summary>
            ExpBegin,
            /// <summary> Has first operand, needs operator </summary>
            NeedOperator,
            /// <summary> Has first operand and binary operator, needs second operand </summary>
            NeedSecondOperand,

            /// <summary> Something goes wrong </summary>
            Err,
        }

        class Operation {
            public Operation(string operation, int priority) {
                this.op = operation;
                this.priority = priority;
            }

            public string op;
            public int priority;
        }

        /// <summary>
        /// State transition matrix for RPN state automaton
        /// </summary>
        static RPNState[][] stateMachine = new RPNState[][] {
            // RPNState.ExpBegin
            new RPNState[] { RPNState.NeedOperator, RPNState.NeedOperator, RPNState.NeedOperator, RPNState.NeedOperator, RPNState.NeedOperator,
                RPNState.ExpBegin, RPNState.ExpBegin, RPNState.ExpBegin, RPNState.ExpBegin, RPNState.ExpBegin, RPNState.ExpBegin
            },

            // RPNState.NeedOperator
            new RPNState[] { RPNState.Err, RPNState.Err, RPNState.Err, RPNState.Err, RPNState.Err,
                RPNState.Err, RPNState.NeedOperator, RPNState.NeedSecondOperand, RPNState.Err, RPNState.Err, RPNState.ExpBegin
            },

            // RPNState.NeedSecondOperand
            new RPNState[] { RPNState.NeedOperator, RPNState.NeedOperator , RPNState.NeedOperator , RPNState.NeedOperator , RPNState.NeedOperator ,
                RPNState.ExpBegin, RPNState.NeedOperator, RPNState.NeedOperator, RPNState.NeedSecondOperand, RPNState.ExpBegin, RPNState.Err
            },

            // RPNState.Err
            new RPNState[] { RPNState.Err, RPNState.Err , RPNState.Err , RPNState.Err , RPNState.Err , RPNState.Err , RPNState.Err , RPNState.Err , RPNState.Err, RPNState.Err, RPNState.Err }
        };

        private static Dictionary<string, OpCode> opToCodeDict = new Dictionary<string, OpCode> {
            { "+", OpCodes.Add },
            { "-", OpCodes.Sub },
            { "/", OpCodes.Div },
            { "*", OpCodes.Mul },
            { "~", OpCodes.Neg },
        };

        private static Dictionary<string, int> priorities = new Dictionary<string, int> {
            { ")", 0 },
            { "(", 1 },
            { "+", 3 },
            { "-", 3 },
            { "*", 4 },
            { "/", 4 },
            { "~", 5 },
            { ",", 2 }
        };

        private static readonly HashSet<char> stopChars = new HashSet<char>() {
            ' ', '(', ')', '+', '-', '/', '*', ','
        };

        private static readonly BindingFlags bindingFlagMask = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static;

        private static int GetPriority(string operation) {
            if (priorities.ContainsKey(operation)) {
                return priorities[operation];
            }

            return 1;
        }

        private static TokenType GetWordType(string word) {
            if (word == "x") {
                return TokenType.X;
            } else if (word == "y") {
                return TokenType.Y;
            } else if (word == "z") {
                return TokenType.Z;
            } else if (word == "(") {
                return TokenType.BracketsOpen;
            } else if (word == ")") {
                return TokenType.BracketsClose;
            } else if (word == "~") {
                return TokenType.UnaryOperator;
            } else if (word == ",") {
                return TokenType.Comma;
            }

            bool isNumber = true;
            bool isName = true; ;

            for (int i = 0; i < word.Length; i++) {
                if (!char.IsDigit(word[i])) {
                    isNumber = false;
                }

                if (!char.IsLetterOrDigit(word[i])) {
                    isName = false;
                }
            }

            if (word.Length > 1 && word[^1] == '(' || word.Length > 2 && word[^2] == '(') {
                return TokenType.Function;
            }

            if (isNumber) {
                return TokenType.Number;
            }

            if (isName) {
                return TokenType.Variable;
            }

            return TokenType.BinaryOperator;
        }

        /// <summary>
        /// Split string expression in infix notation into list of tokens
        /// </summary>
        /// <param name="expression"> Infix expression to split </param>
        /// <returns>List of tokens</returns>
        private static List<string> Tokenize(string expression) {
            var tokens = new List<string>();

            if (expression.Length == 0) {
                return tokens;
            }

            var curToken = string.Empty;

            for (int i = 0; i < expression.Length - 1; i++) {
                // Space skipping
                while (i < expression.Length - 2 && expression[i] == ' ') {
                    i++;
                }

                if (expression[i] == ' ') {
                    break;
                }

                curToken += expression[i];

                // End of token found
                if (stopChars.Contains(expression[i]) || stopChars.Contains(expression[i + 1])) {
                    // Function detection

                    var tokenType = GetWordType(curToken);

                    if (tokenType == TokenType.Variable && i < expression.Length - 2 && expression[i + 1] == '(') {
                        curToken += "(";
                        i += 1;
                    }

                    tokens.Add(curToken);
                    curToken = string.Empty;
                }
            }

            // Cheking for last token
            if (expression[^1] != ' ') {
                curToken += expression[^1];
            }

            if (curToken != string.Empty) {
                tokens.Add(curToken);
            }

            return tokens;
        }

        /// <summary>
        /// Converts list of tokens of expression written by infix notation into postfix notation (RPN)
        /// </summary>
        /// <param name="tokens"> Tokens of infix expression</param>
        /// <returns>List of tokens in postfix order</returns>
        private static List<string> ConvertToRPN(List<string> tokens) {
            tokens.Add(")");

            var resultTokens = new List<string>();
            var curState = RPNState.ExpBegin;

            int curBracketsLevel = 0;

            // Stack for operators
            var operationStack = new Stack<Operation>();

            // Stack for function arguments counts
            var functionStack = new Stack<int>();

            int argsCnt = 0;

            foreach (var token in tokens) {
                string curToken = token;
                if (curToken == "-" && (curState == RPNState.ExpBegin || curState == RPNState.NeedSecondOperand)) {
                    curToken = "~";
                }

                var tokenType = GetWordType(curToken);

                if (tokenType >= TokenType.BracketsOpen) {
                    if (curToken[^1] == '(') {
                        curBracketsLevel++;
                    }

                    int priority = GetPriority(curToken) + 10 * curBracketsLevel;

                    if (curToken != "(") {
                        // Pop all operators with higher priority
                        while (operationStack.Count > 0 && operationStack.Peek().priority >= priority) {
                            if (operationStack.Peek().op[^1] == '(') {
                                if (GetWordType(operationStack.Peek().op) == TokenType.Function) {

                                    // function has more than zero arguments
                                    if (curState == RPNState.NeedOperator) {
                                        functionStack.Push(functionStack.Pop() + 1);
                                    }

                                    operationStack.Peek().op += (char)functionStack.Pop();
                                    resultTokens.Add(operationStack.Peek().op);
                                }

                                operationStack.Pop();
                            } else {
                                resultTokens.Add(operationStack.Pop().op);
                            }
                        }
                    }

                    if (curToken != ")" ) {
                        if (curToken == ",") {
                            // New argument for current function found
                            functionStack.Push(functionStack.Pop() + 1);
                        } else {
                            if (tokenType == TokenType.Function) {
                                functionStack.Push(0);
                            }

                            operationStack.Push(new Operation(curToken, priority));
                        }
                    } else {
                        curBracketsLevel--;
                    }
                } else {
                    argsCnt++;
                    resultTokens.Add(curToken);
                }

                curState = stateMachine[(int)curState][(int)tokenType];

                if (curState == RPNState.Err) {
                    throw new Exception("Incorrect expression to parse");
                }
            }

            return resultTokens;
        }

        static CompileResult Compile(string expression) {
            Type[] args = { typeof(long), typeof(long), typeof(long) };

            var resMethod = new DynamicMethod("ResultMethod", typeof(long), args);

            var ilGenerator = resMethod.GetILGenerator();

            List<string> tokens = Tokenize(expression);

            if (tokens.Count == 0) {
                long tmp = 0;
                ilGenerator.Emit(OpCodes.Ldc_I8, tmp);
                ilGenerator.Emit(OpCodes.Ret);
                return resMethod.CreateDelegate(typeof(CompileResult)) as CompileResult;
            }

            // Convert to RPN

            tokens = ConvertToRPN(tokens);

            // Compile to IL

            foreach (var curToken in tokens) {
                TokenType tokenType = GetWordType(curToken);

                switch (tokenType) {
                    case TokenType.BinaryOperator:
                    case TokenType.UnaryOperator:
                        ilGenerator.Emit(opToCodeDict[curToken]);
                        break;
                    case TokenType.Function: {
                            int curArgs = curToken[^1];

                            Type[] argsTypes = null;

                            var callerClass = new StackTrace().GetFrame(1).GetMethod().DeclaringType;
                            var methodName = curToken[0..^2];

                            if (curArgs > 0) {
                                argsTypes = new Type[curArgs];
                                for (int j = 0; j < curArgs; j++) {
                                    argsTypes[j] = typeof(long);
                                }

                                var method = callerClass.GetMethod(methodName, bindingFlagMask, null, CallingConventions.Any, argsTypes, null);

                                if (method == null) {
                                    throw new Exception($"Can't find method {methodName} in {callerClass.Name}");
                                }

                                ilGenerator.EmitCall(OpCodes.Call, method, null);
                                curArgs = 0;
                                break;
                            }

                            var tmp = callerClass.GetMethod(curToken[0..^2], bindingFlagMask);
                            if (tmp == null) {
                                throw new Exception($"Can't find method {methodName} in {callerClass.Name}");
                            }

                            ilGenerator.EmitCall(OpCodes.Call, tmp, null);

                            break;
                        }
                    case TokenType.Number:
                        var curNum = Int64.Parse(curToken);
                        ilGenerator.Emit(OpCodes.Ldc_I8, curNum);
                        break;
                    case TokenType.X:
                        ilGenerator.Emit(OpCodes.Ldarg_0);
                        break;
                    case TokenType.Y:
                        ilGenerator.Emit(OpCodes.Ldarg_1);
                        break;
                    case TokenType.Z:
                        ilGenerator.Emit(OpCodes.Ldarg_2);
                        break;
                    case TokenType.Variable: {
                            var callerClass = new StackTrace().GetFrame(1).GetMethod().DeclaringType;
                            var curField = callerClass.GetField(curToken, bindingFlagMask);
                            if (curField == null) {
                                throw new Exception($"Can't find field {curToken} in {callerClass.Name}");
                            }

                            ilGenerator.Emit(OpCodes.Ldc_I8, (long)curField.GetValue(null));
                            break;
                        }
                    case TokenType.Comma:
                    default:
                        throw new Exception("Something goes wrong");
                }
            }

            ilGenerator.Emit(OpCodes.Ret);

            return resMethod.CreateDelegate(typeof(CompileResult)) as CompileResult;
        }
    }
}
