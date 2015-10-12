/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.parser.err.ErrBase;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

public class RawError <E extends Enum<?>>
{
  private int pos;
  private int line;
  private int col;
  private List<E> nonterminals;
  private List<ErrBase> failedMatches;
  private E currentNT;

  public RawError (int pos, List<E> nonterminals, List<ErrBase> failedMatches, E currentNT) {
    this.pos = pos;
    this.nonterminals = nonterminals;
    this.failedMatches = failedMatches;
    this.currentNT = currentNT;
  }

  public RawError clone () {
    return new RawError(pos, new ArrayList<E>(nonterminals), new ArrayList<ErrBase>(failedMatches), currentNT);
  }

  private List<String> uniq () {
    ArrayList<String> newList = new ArrayList<String>();

    for (E nt : nonterminals) {
      if (newList.indexOf(nt.toString()) == -1) {
        newList.add(nt.toString());
      }
    }

    newList.sort(new Comparator<String>() {
      public int compare (String x, String y) {
        return x.compareTo(y);
      }
    });

    return newList;
  }

  private void getLineCol (int pos, char[] input) {
    int lastLineBreak = 0, i;
    line = 1;
    for (i = 0; i < pos; i++) {
      if ((i+1) < pos && input[i] == '\r' && input[i+1] == '\n') {
        continue;
      }
      if (input[i] == '\r' || input[i] == '\n') {
        line++;
        lastLineBreak = i + 1;
      }
    }
    col = i - lastLineBreak;
  }

  public ParseError toParseError (char[] input) {
    getLineCol(pos, input);
    Collections.reverse(failedMatches);
    return new ParseError(pos, line, col, uniq(), failedMatches);
  }

  public int getPos() {
    return pos;
  }

  public List<E> getNonterminals () {
    return nonterminals;
  }

  public E getCurrentNT () {
    return currentNT;
  }

  public List<ErrBase> getFailedMatches () {
    return failedMatches;
  }
}
