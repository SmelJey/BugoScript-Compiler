using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

namespace Compile2 {
    public sealed class Compiler {
        public delegate long CompileResult();
        public delegate long ILProgram();

        /// <summary>
        /// Tokens in expression compilation
        /// </summary>
        private enum TokenType {
            Boolean,
            Variable,
            Number,
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
        private enum RPNState {
            /// <summary>  Expression just started </summary>
            ExpBegin,
            /// <summary> Has first operand, needs operator </summary>
            NeedsOperator,
            /// <summary> Has first operand and binary operator, needs second operand </summary>
            NeedsSecondOperand,

            /// <summary> Something goes wrong </summary>
            Err,
        }

        /// <summary>
        /// State of compilator's state machine
        /// </summary>
        private enum CompilatorState {
            /// <summary> Block just started </summary>
            BlockBegin,
            /// <summary> Block name received, needs open bracket </summary>
            NeedsBlockOpen,
            /// <summary> Variable name received, needs assign operator </summary>
            NeedsAssign,
            /// <summary> Single expression compilation state </summary>
            NeedsExpression,
            /// <summary> Something goes wrong </summary>
            Err
        }

        /// <summary> Compilator's lexemes </summary>
        private enum LexemeType {
            BracketsOpen,
            BracketsClose,
            BlockName,
            Name,
            Semicolon,
            Assign,
            FunctionCall,
            Return,
        }

        private sealed class Operation {
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
        private static readonly RPNState[][] stateMachine = new RPNState[][] {
            // RPNState.ExpBegin
            new RPNState[] {
                RPNState.NeedsOperator, RPNState.NeedsOperator, RPNState.NeedsOperator,
                RPNState.ExpBegin, RPNState.ExpBegin,
                RPNState.ExpBegin, RPNState.ExpBegin,
                RPNState.ExpBegin, RPNState.ExpBegin
            },

            // RPNState.NeedOperator
            new RPNState[] {
                RPNState.Err, RPNState.Err, RPNState.Err,
                RPNState.Err, RPNState.NeedsOperator,
                RPNState.NeedsSecondOperand, RPNState.Err,
                RPNState.Err, RPNState.ExpBegin
            },

            // RPNState.NeedSecondOperand
            new RPNState[] {
                RPNState.NeedsOperator, RPNState.NeedsOperator, RPNState.NeedsOperator,
                RPNState.ExpBegin, RPNState.Err,
                RPNState.NeedsOperator, RPNState.NeedsSecondOperand,
                RPNState.ExpBegin, RPNState.Err
            },

            // RPNState.Err
            new RPNState[] { RPNState.Err, RPNState.Err, RPNState.Err, RPNState.Err, RPNState.Err, RPNState.Err, RPNState.Err, RPNState.Err, RPNState.Err }
        };

        /// <summary>
        /// Compilator's state transitions
        /// </summary>
        private static readonly CompilatorState[][] compilationMachine = new CompilatorState[][] {
            // CompilatorState.BlockBegin
            new CompilatorState[] {
                CompilatorState.Err, CompilatorState.BlockBegin,
                CompilatorState.NeedsBlockOpen, CompilatorState.NeedsAssign,
                CompilatorState.BlockBegin, CompilatorState.Err,
                CompilatorState.NeedsExpression, CompilatorState.NeedsExpression,
            },

            // CompilatorState.NeedBlockOpen
            new CompilatorState[] {
                CompilatorState.BlockBegin, CompilatorState.Err,
                CompilatorState.Err, CompilatorState.Err,
                CompilatorState.Err, CompilatorState.Err,
                CompilatorState.Err, CompilatorState.Err
            },

            // CompilatorState.NeedsAssign
            new CompilatorState[] {
                CompilatorState.Err, CompilatorState.Err,
                CompilatorState.Err, CompilatorState.Err,
                CompilatorState.Err, CompilatorState.NeedsExpression,
                CompilatorState.Err, CompilatorState.Err
            },

            // CompilatorState.NeedsExpression
            new CompilatorState[] {
                CompilatorState.Err, CompilatorState.Err,
                CompilatorState.Err, CompilatorState.NeedsExpression,
                CompilatorState.BlockBegin, CompilatorState.Err,
                CompilatorState.NeedsExpression, CompilatorState.Err
            },

            // CompilatorState.Err
            new CompilatorState[] {
                CompilatorState.Err, CompilatorState.Err,
                CompilatorState.Err, CompilatorState.Err,
                CompilatorState.Err, CompilatorState.Err,
                CompilatorState.Err, CompilatorState.Err
            }
        };

        private static readonly Dictionary<string, OpCode[]> opToCodeDict = new Dictionary<string, OpCode[]> {
            { "+", new OpCode[]{ OpCodes.Add } },
            { "-", new OpCode[]{ OpCodes.Sub } },
            { "/", new OpCode[]{ OpCodes.Div } },
            { "*", new OpCode[]{ OpCodes.Mul } },
            { "~", new OpCode[]{ OpCodes.Neg } },
            { "==", new OpCode[]{ OpCodes.Ceq } },
            { ">", new OpCode[]{ OpCodes.Cgt } },
            { "<", new OpCode[]{ OpCodes.Clt } },
            { "!=", new OpCode[]{ OpCodes.Ceq, OpCodes.Ldc_I4_0, OpCodes.Ceq } },
            { ">=", new OpCode[]{ OpCodes.Clt, OpCodes.Ldc_I4_0, OpCodes.Ceq }},
            { "<=", new OpCode[]{ OpCodes.Cgt, OpCodes.Ldc_I4_0, OpCodes.Ceq }},
            { "!", new OpCode[]{ OpCodes.Ldc_I4_0, OpCodes.Ceq }},
            { "&&", new OpCode[]{ OpCodes.And } },
            { "||", new OpCode[]{ OpCodes.Or } },
        };

        private static readonly Dictionary<string, int> priorities = new Dictionary<string, int> {
            { ")", 0 }, { "(", 1 },
            { "+", 7 }, { "-", 7 },
            { "*", 8 }, { "/", 8 },
            { "~", 9 }, { "!", 9 },
            { "==", 5 }, { "!=", 5 },
            { ">", 6 }, { "<", 6 },
            { ">=", 6 }, { "<=", 6 },
            { "&&", 4 }, { "||", 3 },
            { ",", 2 }
        };

        /// <summary>
        /// Contains information of operators: arguments types and return type
        /// </summary>
        private static readonly Dictionary<string, TokenType[]> operatorsInfo = new Dictionary<string, TokenType[]>() {
            { "+", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Number } },
            { "-", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Number } },
            { "*", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Number } },
            { "/", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Number } },
            { "~", new TokenType[] { TokenType.Number, TokenType.Number } },
            { "==", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Boolean } },
            { "!=", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Boolean } },
            { ">", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Boolean } },
            { "<", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Boolean } },
            { ">=", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Boolean } },
            { "<=", new TokenType[] { TokenType.Number, TokenType.Number, TokenType.Boolean } },
            { "&&", new TokenType[] { TokenType.Boolean, TokenType.Boolean, TokenType.Boolean } },
            { "||", new TokenType[] { TokenType.Boolean, TokenType.Boolean, TokenType.Boolean } },
            { "!", new TokenType[] { TokenType.Boolean, TokenType.Boolean } }
        };

        private static readonly HashSet<char> stopChars = new HashSet<char>() {
            ' ', '(', ')', '+', '-', '/', '*', ',',
            ';', '{', '}', '<', '>', '='
        };

        private static readonly HashSet<string> reservedNames = new HashSet<string>() {
            "declare", "program", "if", "else", "true", "false"
        };

        private static readonly BindingFlags bindingFlagMask = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static;

        private static int GetPriority(string operation) {
            if (priorities.ContainsKey(operation)) {
                return priorities[operation];
            }

            return 1;
        }

        private static readonly Dictionary<string, TokenType> tokenDict = new Dictionary<string, TokenType>() {
            { "(", TokenType.BracketsOpen },
            { ")", TokenType.BracketsClose },
            { "~", TokenType.UnaryOperator },
            { "!", TokenType.UnaryOperator },
            { ",", TokenType.Comma },
            { "true", TokenType.Boolean },
            { "false", TokenType.Boolean },
        };

        private static readonly Dictionary<string, LexemeType> lexemesDict = new Dictionary<string, LexemeType>() {
            { "{", LexemeType.BracketsOpen },
            { "}", LexemeType.BracketsClose },
            { "declare", LexemeType.BlockName },
            { "program", LexemeType.BlockName },
            { "if", LexemeType.BlockName },
            { "else", LexemeType.BlockName },
            { "=", LexemeType.Assign },
            { ";", LexemeType.Semicolon },
            { "return", LexemeType.Return },
        };

        private static TokenType GetTokenType(string word) {
            if (tokenDict.ContainsKey(word)) {
                return tokenDict[word];
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

        private static LexemeType GetLexemeType(string lexeme) {
            if (lexemesDict.ContainsKey(lexeme)) {
                return lexemesDict[lexeme];
            }
            if (GetTokenType(lexeme) == TokenType.Function) {
                return LexemeType.FunctionCall;
            }

            return LexemeType.Name;
        }

        /// <summary>
        /// Split string expression in infix notation into list of tokens
        /// </summary>
        /// <param name="expression"> Infix expression to split </param>
        /// <returns> List of tokens </returns>
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

                    var tokenType = GetTokenType(curToken);

                    if (tokenType == TokenType.Variable && i < expression.Length - 2 && expression[i + 1] == '(' && curToken != "if") {
                        curToken += "(";
                        i++;
                    }

                    if ((curToken == "=" || curToken == ">" || curToken == "<" || curToken == "!") && i < expression.Length - 2
                        && (expression[i + 1] == '=' || expression[i + 1] == '>' || expression[i + 1] == '<')) {
                        curToken += expression[i + 1];
                        i++;
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
        /// Converts sublist of tokens from <paramref name="start"/> to <paramref name="end"/> of expression written in infix notation into postfix notation (RPN).
        /// Converts all useless tokens into string.Empty
        /// </summary>
        /// <param name="tokens"> Tokens of infix expression</param>
        /// <param name="start"> First index of sublist </param>
        /// <param name="end"> Last index of sublist (inclusive) </param>
        /// <returns>List of tokens in postfix order</returns>
        private static void ConvertToRPN(List<string> tokens, int start, int end) {
            var resultTokens = new List<string>();
            var curState = RPNState.ExpBegin;

            int curBracketsLevel = 0;

            // Stack for operators
            var operationStack = new Stack<Operation>();

            // Stack for function arguments counts
            var functionStack = new Stack<int>();

            for (int i = start; i <= end + 1; i++) {
                if (curBracketsLevel < 0) {
                    throw new Exception("Incorrect brackets sequence");
                }
                
                // Virtual last element of sublist
                string curToken = ")";
                if (i <= end) {
                    curToken = tokens[i];
                }

                // Unary minus detection
                if (curToken == "-" && (curState == RPNState.ExpBegin || curState == RPNState.NeedsSecondOperand)) {
                    curToken = "~";
                }

                var tokenType = GetTokenType(curToken);

                // Operators processing
                if (tokenType >= TokenType.BracketsOpen) {
                    if (curToken[^1] == '(') {
                        curBracketsLevel++;
                    }

                    int priority = GetPriority(curToken) + 10 * curBracketsLevel;

                    if (curToken != "(") {
                        // Pop all operators with higher priority
                        while (operationStack.Count > 0 && operationStack.Peek().priority >= priority) {
                            string curOperation = operationStack.Peek().op;
                            if (curOperation[^1] == '(') {
                                if (GetTokenType(curOperation) == TokenType.Function) {
                                    int argsCount = functionStack.Pop();

                                    // function has more than zero arguments
                                    if (curState == RPNState.NeedsOperator) {
                                        argsCount++;
                                    }

                                    curOperation += (char)argsCount;
                                }
                            }

                            if (curOperation == "(") {
                                curOperation = string.Empty;
                            }

                            resultTokens.Add(curOperation);
                            operationStack.Pop();
                        }
                    }

                    if (curToken == ",") {
                        // New argument for current function found
                        functionStack.Push(functionStack.Pop() + 1);
                        resultTokens.Add(string.Empty);
                    } else if (curToken == ")" ) {
                        curBracketsLevel--;
                        resultTokens.Add(string.Empty);
                    } else {
                        if (tokenType == TokenType.Function) {
                            functionStack.Push(0);
                        }

                        operationStack.Push(new Operation(curToken, priority));
                    }
                } else {
                    resultTokens.Add(curToken);
                }

                curState = stateMachine[(int)curState][(int)tokenType];

                if (curState == RPNState.Err) {
                    throw new Exception("Incorrect expression to parse");
                }
            }

            if (curBracketsLevel != -1) {
                throw new Exception("Incorrect brackets sequence");
            }

            for (int i = start; i <= end; i++) {
                tokens[i] = resultTokens[i - start];
            }
        }

        /// <summary>
        /// Compiles tokens from <paramref name="start"/> (inclusive) to <paramref name="end"/> (inclusive) into bytecode of <paramref name="ilGenerator"/>
        /// </summary>
        /// <param name="ilGenerator"> IL Generator from Compile method</param>
        /// <param name="tokens">List of tokens</param>
        /// <param name="start">Beginning of expression</param>
        /// <param name="end">End of expression</param>
        public static void CompileExpression(ILGenerator ilGenerator, List<string> tokens, int start, int end, Type returnType, Dictionary<string, LocalBuilder> localVars = null) {
            if (returnType != typeof(long) && returnType != typeof(bool) && returnType != typeof(void)) {
                throw new Exception("Expression of this return type is not supported");
            }

            if (end - start < 0) {
                long tmp = 0;
                ilGenerator.Emit(OpCodes.Ldc_I8, tmp);
                return;
            }

            ConvertToRPN(tokens, start, end);

            // Compile to IL
            var compilationStack = new Stack<TokenType>();

            for (int i = start; i < end + 1; i++) {
                var curToken = tokens[i];

                // Ugly thing to make code work after remaking of ConvertToRPN()
                if (curToken == string.Empty) {
                    continue;
                }

                TokenType tokenType = GetTokenType(curToken);

                switch (tokenType) {
                    case TokenType.BinaryOperator:
                    case TokenType.UnaryOperator: {
                            for (int j = operatorsInfo[curToken].Length - 2; j >= 0; j--) {
                                if (compilationStack.Count == 0) {
                                    throw new Exception($"Not enough arguments for operator <{curToken}>");
                                }

                                if (compilationStack.Pop() != operatorsInfo[curToken][j]) {
                                    throw new Exception($"Invalid argument type for operator <{curToken}> (need {operatorsInfo[curToken][j]})");
                                }
                            }

                            compilationStack.Push(operatorsInfo[curToken][^1]);
                            foreach (OpCode opCode in opToCodeDict[curToken]) {
                                ilGenerator.Emit(opCode);
                            }

                            break;
                        }
                    case TokenType.Function: {
                            int curArgs = curToken[^1];

                            var callerClass = new StackTrace().GetFrame(2).GetMethod().DeclaringType;
                            var methodName = curToken[0..^2];

                            MethodInfo method;

                            if (curArgs > 0) {
                                Type[] argsTypes = new Type[curArgs];
                                for (int j = 0; j < curArgs; j++) {
                                    argsTypes[j] = typeof(long);

                                    if (compilationStack.Count == 0) {
                                        throw new Exception($"Not enough arguments for function <{methodName}>");
                                    }

                                    if (compilationStack.Pop() != TokenType.Number) {
                                        throw new Exception($"Invalid argument type for function <{methodName}>");
                                    }
                                }

                                method = callerClass.GetMethod(methodName, bindingFlagMask, null, CallingConventions.Any, argsTypes, null);
                            } else {
                                method = callerClass.GetMethod(curToken[0..^2], bindingFlagMask);
                            }

                            if (method == null) {
                                throw new Exception($"Can't find method {methodName} in {callerClass.Name}");
                            }

                            if (method.ReturnType == typeof(long)) {
                                compilationStack.Push(TokenType.Number);
                            } else {
                                if (method.ReturnType == typeof(void) && returnType != typeof(void)) {
                                    throw new Exception("Void function in non-void expressions are not supported");
                                }

                                if (method.ReturnType != typeof(void) && method.ReturnType != typeof(long)) {
                                    throw new Exception($"Functions with {method.ReturnType.Name} return type are not supported");
                                }
                            }

                            ilGenerator.EmitCall(OpCodes.Call, method, null);
                            break;
                        }
                    case TokenType.Number: {
                            var curNum = Int64.Parse(curToken);
                            ilGenerator.Emit(OpCodes.Ldc_I8, curNum);
                            compilationStack.Push(TokenType.Number);
                            break;
                        }
                    case TokenType.Variable: {
                            if (localVars != null && localVars.ContainsKey(curToken)) {
                                ilGenerator.Emit(OpCodes.Ldloc, localVars[curToken]);
                                compilationStack.Push(TokenType.Number);
                                break;
                            }

                            var callerClass = new StackTrace().GetFrame(2).GetMethod().DeclaringType;
                            var curField = callerClass.GetField(curToken, bindingFlagMask);
                            if (curField == null) {
                                throw new Exception($"Can't find field {curToken} in {callerClass.Name}");
                            }

                            ilGenerator.Emit(OpCodes.Ldc_I8, (long)curField.GetValue(null));
                            compilationStack.Push(TokenType.Number);
                            break;
                        }
                    case TokenType.Boolean: {
                            long val = 1;
                            if (curToken == "false") {
                                val = 0;
                            }

                            ilGenerator.Emit(OpCodes.Ldc_I8, val);
                            compilationStack.Push(TokenType.Boolean);
                            break;
                        }
                    default:
                        throw new Exception("Incorrect token found");
                }
            }

            if (compilationStack.Count == 0) {
                if (returnType != typeof(void)) {
                    throw new Exception($"Invalid return value: need <{returnType.Name}>, found <void>");
                }
                
            } else {
                if (compilationStack.Peek() == TokenType.Boolean && returnType != typeof(bool) && returnType != typeof(void)) {
                    throw new Exception($"Invalid return value: need <{returnType.Name}>, found <bool>");
                }

                if (compilationStack.Peek() == TokenType.Number && returnType != typeof(long) && returnType != typeof(void)) {
                    throw new Exception($"Invalid return value: need <{returnType.Name}>, found <long>");
                }
            }

            return;
        }

        private static bool ValidateName(string name, Dictionary<string, LocalBuilder> localVars) {
            if (localVars.ContainsKey(name)) {
                throw new Exception($"Var {name} is already exist");
            }

            if (reservedNames.Contains(name)) {
                throw new Exception($"Name {name} is reserved");
            }

            bool isValid = true;
            foreach (var c in name) {
                isValid &= char.IsLetterOrDigit(c);
            }

            isValid &= char.IsLetter(name[0]);
            return isValid;
        } 

        public static ILProgram Compile(string code) {
            var program = new DynamicMethod("SimpleProgram", typeof(long), null);
            var ilGenerator = program.GetILGenerator();

            List<string> tokens = Tokenize(code);

            if (tokens.Count == 0) {
                long tmp = 0;
                ilGenerator.Emit(OpCodes.Ldc_I8, tmp);
                ilGenerator.Emit(OpCodes.Ret);
                return program.CreateDelegate(typeof(ILProgram)) as ILProgram;
            }

            var curState = CompilatorState.BlockBegin;

            var blockStack = new Stack<string>();

            // Stack for if-else statements
            var ifLabelStack = new Stack<Label>();

            // Stack for cheching return statements in if-else blocks
            var returnStack = new Stack<bool>();

            
            int curExpressionStart = 0;
            string assignmentVar = string.Empty;
            bool isIfExpression = false;

            var localVars = new Dictionary<string, LocalBuilder>();

            int programBlocks = 0;

            for (int i = 0; i < tokens.Count; i++) {
                // Process expression for variable assignment / function call
                if (curState == CompilatorState.NeedsExpression || isIfExpression) {
                    while (i < tokens.Count && ((!isIfExpression && tokens[i] != ";") || (isIfExpression && tokens[i] != "{"))) {
                        i++;
                    }

                    if (i >= tokens.Count || (!isIfExpression && tokens[i] != ";") || (isIfExpression && tokens[i] != "{")) {
                        throw new Exception("Semicolon is missing");
                    }

                    Type returnType = typeof(void);
                    if (isIfExpression) {
                        returnType = typeof(bool);
                    } else if (assignmentVar != string.Empty) {
                        returnType = typeof(long);
                    }

                    CompileExpression(ilGenerator, tokens, curExpressionStart, i - 1, returnType, localVars);

                    // If return or assignment expression
                    if (assignmentVar != string.Empty) {
                        if (!localVars.ContainsKey(assignmentVar)) {
                            if (assignmentVar != "return()") {
                                throw new Exception($"Local var {assignmentVar} does not exist");
                            }

                            ilGenerator.Emit(OpCodes.Ret);
                        } else {
                            ilGenerator.Emit(OpCodes.Stloc, localVars[assignmentVar]);
                        }

                        assignmentVar = string.Empty;
                    }

                    if (isIfExpression) {
                        ifLabelStack.Push(ilGenerator.DefineLabel());
                        ilGenerator.Emit(OpCodes.Brfalse, ifLabelStack.Peek());
                        isIfExpression = false;
                    }
                }

                var curToken = tokens[i];

                LexemeType lexemeType = GetLexemeType(curToken);
                switch (lexemeType) {
                    case LexemeType.BlockName:
                        if (curToken == "declare" && i != 0) {
                            throw new Exception("Declaration block must be first in program");
                        }

                        if (blockStack.Count > 0 && blockStack.Peek() == "declare") {
                            throw new Exception("Only variables are allowed in declaration block");
                        }

                        if (curToken != "declare") {
                            returnStack.Push(false);

                            if (curToken == "program") {
                                programBlocks++;
                            } else if (curToken == "if") {
                                isIfExpression = true;
                                curExpressionStart = i + 1;
                            }
                        }

                        blockStack.Push(curToken);
                        break;

                    case LexemeType.BracketsClose:
                        if (blockStack.Peek() == "if") {
                            if (i < tokens.Count - 1 && tokens[i+1] == "else") {
                                var elseLabel = ilGenerator.DefineLabel();
                                ilGenerator.Emit(OpCodes.Br, elseLabel);
                                ilGenerator.MarkLabel(ifLabelStack.Pop());

                                ifLabelStack.Push(elseLabel);
                            } else {
                                returnStack.Pop();
                                ilGenerator.MarkLabel(ifLabelStack.Pop());
                            }
                        } else if (blockStack.Peek() == "else") {
                            ilGenerator.MarkLabel(ifLabelStack.Pop());
                            bool elseRet = returnStack.Pop();
                            bool ifRet = returnStack.Pop();
                            if (ifRet && elseRet) {
                                returnStack.Pop();
                                returnStack.Push(true);
                            }
                        } else if (blockStack.Peek() == "program") {
                            if (!returnStack.Pop()) {
                                throw new Exception("Not all branches return value");
                            }
                        }

                        blockStack.Pop();
                        break;
                    case LexemeType.FunctionCall:
                        if (blockStack.Count == 0) {
                            throw new Exception("Can't call function outside program block");
                        }

                        if (blockStack.Peek() == "declare") {
                            throw new Exception("Only variables are allowed in declaration block");
                        }

                        curExpressionStart = i;
                        break;
                    case LexemeType.Assign:
                        curExpressionStart = i + 1;
                        assignmentVar = tokens[i - 1];

                        break;
                    case LexemeType.Name:
                        if (blockStack.Count == 0) {
                            throw new Exception("Can't use variables outside program block");
                        }

                        if (blockStack.Peek() == "declare") {
                            if (localVars.ContainsKey(curToken)) {
                                throw new Exception($"Var {curToken} is already exist");
                            }

                            if (reservedNames.Contains(curToken)) {
                                throw new Exception($"Name {curToken} is reserved");
                            }

                            bool isValid = ValidateName(curToken, localVars);

                            if (!isValid) {
                                throw new Exception($"Name {curToken} is not acceptable for a variable");
                            }

                            localVars.Add(curToken, ilGenerator.DeclareLocal(typeof(long)));
                        }

                        break;
                    case LexemeType.Return:
                        if (blockStack.Count == 0) {
                            throw new Exception("Can't use variables outside program block");
                        }

                        curExpressionStart = i + 1;
                        assignmentVar = "return()";

                        returnStack.Pop();
                        returnStack.Push(true);

                        break;
                }

                // Compilator state machine transition
                curState = compilationMachine[(int)curState][(int)lexemeType];

                if (curState == CompilatorState.Err) {
                    throw new Exception("Something goes wrong");
                }
            }

            if (programBlocks != 1) {
                throw new Exception($"Program block must be single (found {programBlocks})");
            }

            if (blockStack.Count > 0) {
                throw new Exception("Block of code is not closed by bracket");
            }

            long inf = long.MaxValue;
            ilGenerator.Emit(OpCodes.Ldc_I8, inf);
            ilGenerator.Emit(OpCodes.Ret);

            return program.CreateDelegate(typeof(ILProgram)) as ILProgram;
        }

        // For testing CompileExpression method (Main task + 1,2 subtasks)
        public static CompileResult CompileSingle(string expression) {
            var resMethod = new DynamicMethod("ResultMethod", typeof(long), null);

            var ilGenerator = resMethod.GetILGenerator();

            List<string> tokens = Tokenize(expression);

            CompileExpression(ilGenerator, tokens, 0, tokens.Count - 1, typeof(long));

            ilGenerator.Emit(OpCodes.Ret);
            return resMethod.CreateDelegate(typeof(CompileResult)) as CompileResult;
        }
    }
}
