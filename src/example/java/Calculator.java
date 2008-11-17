/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008 Orlando D. A. R. Hill
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.List;

import org.waxeye.ast.IAST;
import org.waxeye.ast.IChar;
import org.waxeye.input.InputBuffer;
import org.waxeye.parser.ParseResult;

/**
 * A commandline arithmetic calculator.
 */
public final class Calculator {
    private static final Parser p = new Parser();

    private Calculator() {
    }

    private static Object calc(final String input) {
        final ParseResult<Type> result =
            p.parse(new InputBuffer(input.toCharArray()));
        final IAST<Type> ast = result.getAST();

        if (ast == null) {
            return result.getError();
        }
        else {
            return sum(ast.getChildren().get(0));
        }
    }

    private static double sum(final IAST<Type> sum) {
        final List<IAST<Type>> chil = sum.getChildren();
        double val = prod(chil.get(0));

        for (int i = 1; i < chil.size(); i += 2) {
            final char operator = ((IChar)chil.get(i)).getValue();

            if (operator == '+') {
                val += prod(chil.get(i + 1));
            }
            else {
                val -= prod(chil.get(i + 1));
            }
        }

        return val;
    }

    private static double prod(final IAST<Type> prod) {
        final List<IAST<Type>> chil = prod.getChildren();
        double val = unary(chil.get(0));

        for (int i = 1; i < chil.size(); i += 2) {
            final char operator = ((IChar)chil.get(i)).getValue();

            if (operator == '*') {
                val *= unary(chil.get(i + 1));
            }
            else {
                val /= unary(chil.get(i + 1));
            }
        }

        return val;
    }

    private static double unary(final IAST<Type> unary) {
        switch (unary.getType()) {
            case Unary:
                return - unary(unary.getChildren().get(1));
            case Sum:
                return sum(unary);
            default:
                return num(unary);
        }
    }

    private static double num(final IAST<Type> num) {
        return Double.parseDouble(num.childrenAsString());
    }

    public static void main(String[] args) {
        try {
            final BufferedReader in = new BufferedReader(
                new InputStreamReader(System.in));
            System.out.print("calc> ");
            String line = in.readLine();

            while (line != null) {
                System.out.println(calc(line));
                System.out.print("calc> ");
                line = in.readLine();
            }

            System.out.println();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}
