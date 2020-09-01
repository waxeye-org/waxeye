<?php

# https://www.unicode.org/versions/Unicode13.0.0/
# maximum unicode values: 143859

$charClassGrammarFile = fopen("unicode_charclass.grammar", "w");
$charGrammarFile = fopen("unicode_char.grammar", "w");

for ($i = 0; $i < 143859; $i++) {
    # waxeye cannot work with values between xD800 (55296) and xDFFF (57343)
    if ($i >= 55296 && $i <= 57343) {
        continue;
    }

    $hexValue = dechex($i);
    fwrite($charClassGrammarFile, "uni" . $i . "\t<- [\\u{" . $hexValue . "}]\n");
    fwrite($charGrammarFile, "uni" . $i . "\t<- '\\u{" . $hexValue . "}'\n");
}
fclose($charClassGrammarFile);
fclose($charGrammarFile);
