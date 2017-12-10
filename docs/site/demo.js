"use strict";

(() => {
  const waxeye = window.waxeye;
  const waxeyeCompiler = window.waxeyeCompiler;
  const ui = {};
  let currentParser = null;

  const setCurrentParser = (jsParserSource) => {
    currentParser =
        jsParserSource && new (eval(`(waxeye) => { ${jsParserSource}; return Parser }`)(waxeye));
    parse();
  };

  const parse = () => {
    if (!currentParser) {
      ui.parserOutput.innerText = '';
    } else {
      ui.parseErrorOverlay.strategies = [];
      const result = currentParser.parse(ui.parserInput.value);
      if (result instanceof waxeye.AST) {
        ui.parserOutput.innerText = JSON.stringify(result, null, '  ');
      } else {
        ui.parserOutput.innerText = result.toString();
        if (result instanceof waxeye.ParseError) {
          ui.parseErrorOverlay.strategies = [
            {
              match: matchForOverlayFrom(result.pos),
              css: { 'background-color': '#ffebee' }
            }
          ];
        }
      }
    }
  };

  const safeWaxeyeCompiler = (method, arg) => {
    try {
      return waxeyeCompiler[method](arg)
    } catch (e) {
      return [null, (('stack' in e) ? e.stack : e).toString()];
    }
  };

  const compile = () => {
    const grammar = ui.grammarSource.value;
    const [jsParserSource, jsParserErr] = safeWaxeyeCompiler('generateParser', grammar);
    ui.grammarCompilationError.innerText = jsParserErr || '';

    // Compilation details
    const [out, err] = (() => {
      switch (ui.compilationStatusRadios.find(r => r.checked).value) {
        case 'ast':
          return safeWaxeyeCompiler('grammarToAstString', grammar);
        case 't-ast':
          return safeWaxeyeCompiler('grammarToTransformedAstString', grammar);
        case 'js':
          return [jsParserSource, jsParserErr];
      }
    })();
    ui.grammarCompilationStatusOutput.innerText = out || err;

    setCurrentParser(jsParserSource);
    ui.parseErrorOverlay.render();
  };

  const updateCannedExample = () => {
    const id = ui.cannedSelect.value;
    const [grammar, input] = CANNED_EXAMPLES[id];
    ui.grammarSource.value = grammar;
    ui.parserInput.value = input;
    history.replaceState(null, null, `#${id}`);
    compile();
  };

  const updateURL = () => {
    history.replaceState(null, null, '#' +
      [LZString.compressToEncodedURIComponent(ui.grammarSource.value),
      LZString.compressToEncodedURIComponent(ui.parserInput.value)].join('/'))
  };

  document.addEventListener('DOMContentLoaded', () => {
    ui.cannedSelect = document.querySelector('[name="demo-canned-select"]');
    ui.grammarSource = document.querySelector('[name="demo-grammar-source"]');
    ui.grammarCompilationError = document.querySelector('.demo-grammar-compilation-error-message');
    ui.grammarCompilationStatusOutput = document.querySelector('.demo-grammar-compilation-status-output');
    ui.compilationStatusRadios = Array.from(document.querySelectorAll('[name="compilation-status-tab"]'));
    ui.parserInput = document.querySelector('[name="demo-parser-input"]');
    ui.parserOutput = document.querySelector('.demo-parser-output');

    for (let radio of ui.compilationStatusRadios) {
      radio.addEventListener('change', (e) => {
        if (e.target.checked) {
          compile();
        }
      });
    }
    ui.cannedSelect.addEventListener('change', updateCannedExample);
    ui.grammarSource.addEventListener('input', throttle(compile, 360));
    ui.parserInput.addEventListener('input', parse);
    for (const input of [ui.grammarSource, ui.parserInput]) {
      input.addEventListener('input', throttle(updateURL, 360));
      input.addEventListener('input', () => { ui.cannedSelect.value = '' });
    }
    ui.parseErrorOverlay = new Textoverlay(ui.parserInput, []);

    // Initialize from the URL hash fragment. Cases:
    //
    // 1. #a/b => a is the grammar, b is the input, compressed with LZString.
    // 2. #a => a is the ID of a canned example.
    // 3. Otherwise, load the selected (default) canned example.
    if (window.location.hash.length > 2) {
      if (window.location.hash.indexOf('/') != -1) {
        ui.cannedSelect.value = '';
        const [grammar, input] = window.location.hash.slice(1).split('/').
          map(LZString.decompressFromEncodedURIComponent);
        ui.grammarSource.value = grammar || '';
        ui.parserInput.value = input || '';
        compile();
      } else {
        ui.cannedSelect.value = window.location.hash.slice(1);
        updateCannedExample();
      }
    } else {
      updateCannedExample();
    }
  });

  function matchForOverlayFrom(from) {
    let firstExec = true;
    const matcher = {
      lastIndex: 0,
      exec(input) {
        if (!firstExec) return null;
        firstExec = false;
        matcher.lastIndex = input.length + (from >= input.length ? 1 : 0);
        return { 0: input.slice(from) || " " };
      }
    };
    return matcher;
  }

  const throttle = (f, maxEveryMs) => {
    let last = 0;
    let deferred;
    return (...args) => {
      const now = +new Date();
      if (now >= last + maxEveryMs) {
        last = now;
        f(...args);
      } else {
        clearTimeout(deferred);
        deferred = setTimeout(() => {
          last = now;
          f(...args);
        }, maxEveryMs - (now - last));
      }
    };
  }

  const CANNED_EXAMPLES = {
    calc: [
      `# A grammar for an arithmetic calculator.
# Supports +,-,*,/, negation, parentheses and floating point numbers.

calc  <- ws sum

sum   <- prod *([+-] ws prod)

prod  <- unary *([*/] ws unary)

unary <= '-' ws unary
       | :'(' ws sum :')' ws
       | num

num   <- +[0-9] ?('.' +[0-9]) ws

ws    <: *[ \\t\\n\\r]
`, '10 * (3 + 5)'],
    json: [`# The JSON data format

Json    <- Ws Value


Value   <- ( Object
           | Array
           | Number
           | String
           | Literal)
           Ws


Object  <- :'{' Ws
           ?( Member *(Com Member))
           :'}'


Member  <- String Ws Col Value


Array   <- :'[' Ws
           ?( Value *(Com Value))
           :']'


Number  <- ?'-'
           ('0' | [1-9] *[0-9])
           ?('.' +[0-9])
           ?([eE] ?[+-] +[0-9])


String  <- :'"'
           *( :'\\\\' Escaped
            | !'\\\\' !'"' . )
           :'"'


Escaped <- 'u' [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]
         | ["/\\\\bfnrt]


Literal <- 'true'
         | 'false'
         | 'null'


Ws      <: *[ \\t\\n\\r]


Com     <: ',' Ws


Col     <: ':' Ws
`, '["a", {"cake": "🎂"}]'],
    waxeye: [`# The Waxeye grammar language.

Grammar     <- Ws *Definition


Definition  <- Identifier (LeftArrow | PruneArrow | VoidArrow) Alternation Ws


Alternation <- Sequence *(Alt Sequence)


Sequence    <- +Unit


Unit        <- *(Prefix | Label)
               ( Identifier !(LeftArrow | PruneArrow | VoidArrow)
               | Open Alternation Close
               | Action
               | Literal
               | CaseLiteral
               | CharClass
               | WildCard )


Prefix      <- [?*+:&!] Ws


Label       <- Identifier Ws :'=' Ws


Action      <- :'@' Identifier ?(:'<' Ws Identifier *(Comma Identifier) :'>') Ws


Identifier  <- [a-zA-Z_] *[a-zA-Z0-9_-] Ws


Literal     <- :['] +(!['] (LChar | Hex)) :['] Ws


CaseLiteral <- :["] +(!["] (LChar | Hex)) :["] Ws


LChar       <- '\\\\' [nrt'"\\\\] | !'\\\\' !EndOfLine .


CharClass   <- :'[' *(!']' Range) :']' Ws


Range       <- (Char | Hex) ?(:'-' (Char | Hex))


Char        <- '\\\\' [nrt\\-\\]\\\\] | !'\\\\' !']' !EndOfLine .


Hex         <- :'\\\\u{' [0-9A-Fa-f] ?[0-9A-Fa-f] ?[0-9A-Fa-f] ?[0-9A-Fa-f] ?[0-9A-Fa-f] ?[0-9A-Fa-f] :'}'


WildCard    <- :'.' Ws


LeftArrow   <- :'<-' Ws


PruneArrow  <- :'<=' Ws


VoidArrow   <- :'<:' Ws


#################
# Always voided #
#################

Alt         <: '|' Ws


Open        <: '(' Ws


Close       <: ')' Ws


Comma       <: ',' Ws


SComment    <: '#' *(!EndOfLine .) (EndOfLine | !.)


MComment    <: '/*' *(MComment | !'*/' . ) '*/'


EndOfLine   <: '\\r\\n' | '\\n' | '\\r'


Ws          <: *([ \\t] | EndOfLine | SComment | MComment)
`, `# Non-negative integers

Num <- '0' | [1-9] *[0-9]
`],
  };
})();
