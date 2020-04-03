# BugoScript-Compiler
Simple language with local variables, if-statements, bool and long types, external static methods and variables calling

All tests and language examples are in Tester.cs

You can check OldCompiler.cs for mathematical expressions compiler with search for external static variables and methods

# Syntax
## Blocks
Two main blocks:
+ `declare { <declaration> }` - Declaration block, where all local variables should be declared. Is optional. Supports only long type variables. **Must be the first!**
+ `program { <code> }` - Main execution block. Return is required. **Only one program block should exist.**

Control blocks:
+ `if <condition> { <code> }` - Condtion's brackets are optional. Return type of condition must be bool.
+ `else { <code> }` - Should be always after if statement

**Brackets are required**

## Operators:
+ `<long> +, -, *, / <long>` - returns `<long>`
+ `<long> ==, !=, >=, <=, >, < <long>` - returns `<bool>`
+ `<bool> &&, || <bool>` - returns `<bool>`
+ `!<bool>` - returns `<bool>`
+ `-<long>` - returns `<long>`
+ `<variable> = <expression>;` - special assignment operator.

---

All questions can be asked here: 

Telegram: @SmelJey
