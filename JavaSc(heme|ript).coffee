'use strict'

## Node.js exports (settings)
if exports? # already in Node.js
  colors = require 'colors/safe'
else        # not in Node.js (probably browser)
  exports = {}
exports.colors = false
exports.curry  = true

isa = (instance,classes...) -> classes.some (c) -> instance instanceof c

## Classes
# Symbols
class Symbol
  constructor: (@value) ->
  inspect: (depth)      -> "'"+@value
  @table: {}
  @sym: (str_repr)      -> Symbol.table[str_repr] = Symbol.table[str_repr] ? new Symbol(str_repr)

[_if, _set, _define, _lambda, _begin, _cond, _else, _local, _and, _or, _quote, _unquote] =
'if set! define lambda begin cond else local and or quote unquote'.split(' ').map(Symbol.sym)
[ _unquotesplicing, _quasiquote, _append, _cons, _definemacro, _deletemacro] =
 'unquote-splicing quasiquote append cons define-macro delete-macro'.split(' ').map(Symbol.sym)

# Functions
class Builtin
  constructor: (@func, @expected_args=-1) ->
    if @expected_args == -1 then @expected_args = func.length
  call: (args) ->
    if (@expected_args > args.length) and @expected_args != Infinity
      unless exports.curry
        throw new Error("Function expects #{@expected_args} arguments, not #{to_string(args)}")
      new Curried(@, args)
    else if (@expected_args != args.length) and @expected_args != Infinity
      throw new Error("Function expects #{@expected_args} arguments, not #{to_string(args)}")
    else
      @func.apply(null, args)

class Lambda
  constructor: (@params, @body, @env) ->
    @expected_args = params.length
  call: (args) ->
    if (@expected_args > args.length)
      unless exports.curry
        throw new Error("Function arguments #{to_string(args)} don't match #{to_string(@params)}")
      new Curried(@, args)
    else if (@expected_args != args.length)
      throw new Error("Function arguments #{to_string(args)} don't match #{to_string(@params)}")
    else
      evaluate(@body, new Env(@params, args, @env))

class Curried
  constructor: (@parent, @fixed_args) ->
    @expected_args = parent.expected_args - fixed_args.length
    if isa(parent, Curried)
      @fixed_args = parent.fixed_args.concat fixed_args
      @parent     = parent.parent
  call: (args) ->
    if (@expected_args > args.length)
      new Curried(@parent, @fixed_args.concat(args))
    else
      @parent.call(@fixed_args.concat(args))

# Scopes
class Env
  constructor: (params=[], args=[], @outer) ->
    @dict = {}
    @dict[param.value] = args[i] for param, i in params

  find: (sym) ->
    return this if (sym.value of @dict)
    @outer?.find(sym) ? throw new Error("Could not find Symbol #{to_string(sym)}")

  get_value: (sym)     -> @find(sym).dict[sym.value]
  set_value: (sym,val) -> @find(sym).dict[sym.value] = val
  add_value: (sym,val) -> @dict[sym.value] = val

add_globals = (env) ->
  to_add = {
    # general functions
    'eval': (x) -> evaluate(x)
    'void': undefined, 'true': true, 'false': false, 'empty': []
    'get-definition': (f) ->
      throw new Error("Cannot get definition of #{to_string(f)}") unless isa(f, Lambda)
      [_lambda, f.params, f.body]
    'gensym': -> Symbol.sym(Math.random().toString(36).substring(2))
    'display': (x) -> console.log to_string x
    'to-string': (x) -> if typeof x == 'string' then x else to_string x

    # Predicates
    'procedure?': (x) -> isa(x, Builtin, Lambda, Curried)
    'list?':      (x) -> isa(x, Array)
    'symbol?':    (x) -> isa(x, Symbol)
    'string?':    (x) -> typeof x == 'string'
    'number?':    (x) -> typeof x == 'number'
    'boolean?':   (x) -> typeof x == 'boolean'

    # boolean functions
    'not': (x) ->
      throw new Error("not expected boolean, not #{to_string(x)}") unless (typeof x is 'boolean')
      !x
    'same?': (x,y) -> x == y
    'equal?': equal = (x,y) ->
      return false unless ((type = typeof x) == typeof y)
      if (type == 'object') and x.constructor == y.constructor and x instanceof Array
        return false unless (x.length == y.length)
        return false for i in [0...x.length] unless equal(x[i], y[i])
        return true
      x is y

    # list functions
    'list': new Builtin(((args...) -> args), Infinity)
    'length': (l) ->
      throw new Error("length expected a list, not #{to_string(l)}") unless isa(l, Array)
      l.length
    'first': (l) ->
      throw new Error("first expected a list, not #{to_string(l)}") unless isa(l, Array)
      l[0]
    'rest': (l) ->
      throw new Error("rest expected a list, not #{to_string(l)}") unless isa(l, Array)
      l[1..]
    'cons': (x,l) ->
      unless (l instanceof Array)
        throw new Error("cons expected the second argument to be a list, not #{to_string(l)}")
      [x].concat(l)
    'append': (x,y) ->
      unless (x instanceof Array and y instanceof Array)
        throw new Error("first expected list, not #{to_string(l)}")
      x.concat(y)

    # number functions
    '<': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("< expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      x < y
    '=': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("= expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      x == y
    '+': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("+ expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      x + y
    '-': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("- expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      x - y
    '/': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("/ expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      x / y
    '*': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("* expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      x * y
    'quotient': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("quotient expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      int(x/y)
    'max': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("max expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      Math.max(x,y)
    'min': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("min expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      Math.min(x,y)
    'expt': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("expt expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      Math.pow(x,y)
    'round': (x) ->
      unless (typeof x == 'number')
        throw Error("round expected a numbers, not #{to_string(x)}")
      Math.round(x)
    'remainder': (x,y) ->
      unless (typeof x == typeof y == 'number')
        throw Error("remainder expected two numbers, not #{to_string(x)} and #{to_string(y)}")
      x % y

    # string functions
    'string-append': (x,y) ->
      unless (typeof x == typeof y == 'string')
        throw Error("string-append expected two strings, not #{to_string(x)} and #{to_string(y)}")
      x.concat(y)
    'substring': (x,y,z) ->
      unless (typeof x == 'string' and typeof y == typeof z == 'number')
        throw Error("substring expects a string and two numbers")
      x.substr(y,z-y);
    'string-length': (x) ->
      unless (typeof x == 'string')
        throw Error("string-length expected one strings, not #{to_string(x)}")
      x.length
    'error': (x) ->
      unless (typeof x == 'string')
        throw new Error("error expected a string, not #{to_string(x)}")
      throw new Error(x)
    'print': (x) ->
      unless (typeof x == 'string')
        throw new Error("print expected a string, not #{to_string(x)}")
      console.log x
  }

  for name, value of to_add
    env.dict[name] = if (typeof value == 'function') then new Builtin(value) else value
  env

# Errors
class Error
  constructor: (@message) ->
  @check: (condition, message) -> throw new Error(message) unless condition

## Processing
# Parsing: input string -> s-expression
[to_parse, bracket_balance] = ['', 0];
read_line = (line) ->

  tokenize = (string) ->
    _comments:  /^\s*;[^\n]*(\n\s*|$)/,
    _tokenizer: /^\s*(,@|[()',`]|"(\\.|[^\\"])*"|[^\s)]*)\s*/,
    _remaining: string,
    next: ->
      while (@_remaining.length > 0)
        if @_remaining.match(@_comments,'')?
          @_remaining = @_remaining.replace(@_comments,'')
          continue
        next_token  = @_remaining.match(@_tokenizer)[0].trim()
        @_remaining = @_remaining.replace(@_tokenizer,'')
        return next_token
      null

  parse = (iterator, next_token) ->
    [token, L] = [next_token ? iterator.next(), []]

    switch token
      when null then null
      when '('
        while (token = iterator.next())
          return L if (token is ')')
          L.push(parse(iterator,token))
      when ')'  then throw new Error('Unexpected closing bracket')
      when "'"  then [_quote, parse(iterator)]
      when "`"  then [_quasiquote, parse(iterator)]
      when ","  then [_unquote, parse(iterator)]
      when ",@" then [_unquotesplicing, parse(iterator)]
      else           atom(token)

  atom = (token) ->
    return token.slice(1,-1) if (/^"(\\.|[^\\"])*"$/.test(token))
    return Number(token)     if (/^[+-]?([1-9]\d*|0)(\.\d+)?$/.test(token))
    return true              if (/^(true|True|#t)$/.test(token))
    return false             if (/^(false|False|#f)$/.test(token))
    Symbol.sym(token)

  line = line.replace(/\[|{/g,'(').replace(/\]|}/g, ')')
  to_parse        += line
  bracket_balance += (line.match(/\(/g) ? []).length - (line.match(/\)/g) ? []).length
  return null if (bracket_balance != 0)

  [to_parse, bracket_balance, to_return] = ['', 0, parse(tokenize(to_parse))]
  to_return

# Expanding: s-expression -> s-expression
macro_table = {};
expand = (x) ->

  expand_quasiquote = (x) ->
    if !(x instanceof Array) or (x.length == 0)
      [_quote, x]
    else if x[0] is _unquote
      x[1]
    else if (x[0] instanceof Array) and (x[0].length > 0) and (x[0][0] is _unquotesplicing)
      [_append, x[0][1], expand_quasiquote(x[1..])]
    else
      [_cons, expand_quasiquote(x[0]), expand_quasiquote(x[1..])]

  return x unless (x instanceof Array)

  if (x[0] is _deletemacro)
    delete macro_table[x[1].value]
    undefined
  else if (x[0] in [_define,_definemacro])
    [_def, v, body] = x
    if (v instanceof Array)
      [func, args] = [v[0], v[1..]]
      expand([_def, func, [_lambda, args, body]])
    else
      exp = expand(x[2])
      if _def is _definemacro
        macro_table[v.value] = evaluate(exp)
        undefined
      else
        [_define, v, exp]
  else if (x[0] is _quote)
    x
  else if (x[0] is _quasiquote)
    expand_quasiquote(x[1])
  else if (x[0].value of macro_table)
    expand(macro_table[x[0].value].call(x[1..]))
  else
    x.map(expand)

# Evaluating: s-expression -> result
evaluate = (x, env=global_env) ->
  try
    while true
      if (x instanceof Symbol)
        return env.get_value(x)
      else if not (x instanceof Array) or (not x?)
        return x
      else
        switch x[0]
          when _quote
            Error.check(x.length > 1, "quote expects an argument")
            [arg] = x[1..]
            return arg
          when _if
            Error.check(x.length == 4, "if expects three arguments")
            [cond, if_true, if_false] = x[1..]
            cond = evaluate(cond,env)
            Error.check(typeof (cond = evaluate(cond,env)) == 'boolean',
              "if expects a boolean, not #{cond}")
            x = if cond then if_true else if_false
          when _set
            Error.check(x.length == 3, "set! expects two arguments")
            [symbol, value] = x[1..]
            Error.check(isa(symbol, Symbol), "set! expects a symbol, not #{to_string(symbol)}")
            env.set_value(symbol, evaluate(value,env))
            return undefined
          when _define
            Error.check(x.length == 3, "define expects two arguments")
            [symbol, value] = x[1..]
            Error.check(isa(symbol, Symbol), "define expects a symbol, not #{to_string(symbol)}")
            env.add_value(symbol, evaluate(value,env));
            return undefined;
          when _lambda
            Error.check(x.length == 3, "lambda expects two arguments")
            [params, body] = x[1..]
            Error.check(params.every((s) -> isa(s, Symbol)),
              "lambda expects parameters to be a list of symbols, not #{to_string(params)}")
            return new Lambda(params, body, env)
          when _begin
            [exprs..., x] = x[1..]
            evaluate(expr, env) for expr in exprs
          when _cond
            conds = x[1..]
            Error.check(conds.every((c) -> c.length == 2),
              "expected a list of (<cond> <value>) pairs")
            for [cond, expr] in conds
              if cond is _else
                x = expr
                break
              Error.check(typeof (cond = evaluate(cond,env)) == 'boolean',
                "cond expects a boolean, not #{to_string(cond)}")
              if cond
                x = expr
                break
          when _local
            Error.check(x.length == 3, "local expects two arguments")
            [definitions, x] = x[1..]
            env = new Env([],[],env);
            evaluate(definition, env) for definition in definitions
          when _and
            [args..., x] = x[1..]
            for arg in args
              val = evaluate(arg, env)
              Error.check(typeof val == 'boolean', "and expects booleans, not #{to_string(val)}")
              return false unless val
            true
          when _or
            [args..., x] = x[1..]
            for arg in args
              val = evaluate(arg, env)
              Error.check(typeof val == 'boolean', "or expects booleans, not #{to_string(val)}")
              return true if val
            false
          else
            args = x.map (arg) -> evaluate(arg, env)
            func = args.shift()
            Error.check(isa(func, Builtin, Lambda, Curried),
              "expected a function, not #{to_string(func)}")

            return func.call(args) if isa(func, Builtin) or
                                      isa(func, Curried) and isa(func.parent, Builtin) or
                                      func.expected_args != args.length

            x      = if isa(func, Curried) then func.parent.body else func.body
            params = if isa(func, Curried) then func.parent.params else func.params
            args   = func.fixed_args.concat(args) if isa(func, Curried)
            env    = new Env(params, args, func.env)

  catch e
    throw new Error("#{e.message}\n  in call to #{to_string(x)}") if (e instanceof Error)
    throw e
# Output: result -> ouput string
to_string = (x) ->
  c = (string, color) -> if exports.colors then colors[color](string) else string
  switch
    when isa(x, Array)           then c("(#{x.map(to_string).join(' ')})", 'white')
    when isa(x, Symbol)          then c("'#{x.value}", 'cyan')
    when isa(x, Builtin)         then c("{built-in function}", 'grey')
    when isa(x, Lambda, Curried) then c("{user-defined function}", 'grey')
    when isa(x, Error)           then c("Error: #{x.message}", 'red')
    when x is null               then null
    else switch (typeof x)
          when 'number'  then c(x.toString(), 'yellow')
          when 'boolean' then c(x.toString(), 'magenta')
          when 'string'  then c("\"#{x}\"", 'green')

global_env = add_globals(new Env())

## Node.js exports (exports)
exports.read_line     = read_line
exports.evaluate      = (x) ->
  try evaluate (expand x)
  catch e
    if e instanceof Error then e else new Error("Panic! Javasc(heme|ript) error: #{e}")
exports.to_string     = to_string
exports.pending_lines = -> to_parse
exports.globals       = global_env