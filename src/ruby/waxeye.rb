# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

module Waxeye

  class Edge
    attr_reader :trans, :state, :voided
    def initialize(trans, state, voided)
      @trans = trans
      @state = state
      @voided = voided
    end
  end

  class State
    attr_reader :edges, :match
    def initialize(edges, match)
      @edges = edges
      @match = match
    end
  end

  class FA
    attr_reader :type, :states, :mode
    def initialize(type, states, mode)
      @type = type
      @states = states
      @mode = mode
    end
  end

  class ParseError
    attr_reader :pos, :line, :col, :nt
    def initialize(pos, line, col, nt)
      @pos = pos
      @line = line
      @col = col
      @nt = nt
    end

    def to_s()
      "parse error: failed to match '#{nt}' at line=#{line}, col=#{col}, pos=#{pos}\n"
    end
  end

  class AST
    attr_reader :type, :children, :pos
    def initialize(type, children, pos)
      @type = type
      @children = children
      @pos = pos
    end

    def to_s_sexpr()
      acc = []
      to_s_sexpr_iter(self, acc)
      acc.join('')
    end

    def to_s()
      acc = []
      to_s_iter(self, [0], acc)
      acc.join('')
    end

    private
    def to_s_sexpr_iter(ast, acc)
      acc.push('(')
      acc.push(ast.type)
      ast.children.each do |a|
        acc.push(" ")
        if a.is_a?(Waxeye::AST)
          to_s_sexpr_iter(a, acc)
        else
          acc.push(a)
        end
      end
      acc.push(')')
    end

    def to_s_iter(ast, indent, acc)
      (indent[0] - 1).times {|| acc.push('    ') }
      acc.push('->  ') if indent[0] > 0
      acc.push(ast.type)
      indent[0] += 1
      ast.children.each do |a|
        acc.push("\n")
        if a.is_a?(Waxeye::AST)
          to_s_iter(a, indent, acc)
        else
          (indent[0] - 1).times {|| acc.push('    ') }
          acc.push('|   ') if indent[0] > 0
          acc.push(a)
        end
      end
      indent[0] -= 1
    end
  end

  class WaxeyeParser
    def initialize(start, eof_check, automata)
      @start = start
      @eof_check = eof_check
      @automata = automata
    end

    def parse(input)
      InnerParser.new(@start, @eof_check, @automata, input).parse()
    end

    class InnerParser
      def initialize(start, eof_check, automata, input)
        @start = start
        @eof_check = eof_check
        @automata = automata
        @input = input
        @input_len = input.length
        @input_pos = 0
        @line = 1
        @column = 0
        @last_cr = false
        @error_pos = 0
        @error_line = 1
        @error_col = 0
        @error_nt = automata[start].type
        @fa_stack = []
        @cache = {}
      end

      def parse()
        eof_check(match_automaton(@start))
      end

      private

      def match_automaton(index)
        start_pos = @input_pos
        key = [index, start_pos]

        if (@cache.has_key?(key))
          cachedItem = @cache.fetch(key)
          restore_pos(cachedItem[1], cachedItem[2], cachedItem[3], cachedItem[4])
          return cachedItem[0]
        end

        start_line = @line
        start_col = @column
        start_cr = @last_cr
        automaton = @automata[index]
        type = automaton.type
        mode = automaton.mode

        @fa_stack.push(automaton)
        res = match_state(0)
        @fa_stack.pop()

        value = if type == :_and
                  restore_pos(start_pos, start_line, start_col, start_cr)
                  not not res
                elsif type == :_not
                  restore_pos(start_pos, start_line, start_col, start_cr)
                  if res
                    update_error()
                  else
                    true
                  end
                else
                  if res
                    case mode
                    when :void
                      true
                    when :prune
                      case res.length
                      when 0
                        true
                      when 1
                        res[0]
                      else
                        AST.new(type, res, [start_pos, @input_pos])
                      end
                    else
                      AST.new(type, res, [start_pos, @input_pos])
                    end
                  else
                    update_error()
                  end
                end

        @cache.store(key, [value, @input_pos, @line, @column, @last_cr])
        return value
      end

      def match_state(index)
        state = @fa_stack.last.states[index]
        res = match_edges(state.edges)
        res ? res : (state.match and []) 
      end

      def match_edges(edges)
        if edges == []
          false
        else
          res = match_edge(edges[0])
          res ? res : match_edges(edges[1..-1])
        end
      end

      def match_edge(edge)
        start_pos = @input_pos
        start_line = @line
        start_col = @column
        start_cr = @last_cr
        t = edge.trans
        res = if t == :_wild
                @input_pos < @input_len ? mv() : update_error()
              elsif t.is_a?(String)
                @input_pos < @input_len and t[0] == @input[@input_pos] ? mv() : update_error()
              elsif t.is_a?(Array)
                @input_pos < @input_len and within_set?(t, @input[@input_pos]) ? mv() : update_error()
              elsif t.is_a?(Integer)
                match_automaton(t)
              else
                false
              end

        if res
          tran_res = match_state(edge.state)
          if tran_res
            if edge.voided or res == true
              tran_res
            else
              [res] + tran_res
            end
          else
            restore_pos(start_pos, start_line, start_col, start_cr)
            false
          end
        else
          false
        end
      end

      def restore_pos(pos, line, col, cr)
        @input_pos = pos
        @line = line
        @column = col
        @last_cr = cr
      end

      def update_error()
        if @error_pos < @input_pos
          @error_pos = @input_pos
          @error_line = @line
          @error_col = @column
          @error_nt = @fa_stack.last.type
        end
        false
      end

      def mv()
        ch = @input[@input_pos].chr()
        @input_pos += 1

        if ch == '\r'
          @line += 1
          @column = 0
          @last_cr = true
        else
          if ch == '\n'
            if not @last_cr
              @line += 1
              @column = 0
            end
          else
            @column += 1
          end
          @last_cr = false
        end

        return ch
      end

      def eof_check(res)
        if res
          if @eof_check and @input_pos < @input_len
            # Create a parse error - Not all input consumed
            ParseError.new(@error_pos, @error_line, @error_col, @error_nt)
          else
            res
          end
        else
          # Create a parse error
          ParseError.new(@error_pos, @error_line, @error_col, @error_nt)
        end
      end

      def within_set?(set, c)
        if set == []
          false
        else
          aa = set[0]

          if aa.is_a?(String)
            if aa[0] == c
              true
            else
              aa[0] < c ? within_set?(set[1..-1], c) : false
            end
          else
            # If not a String then must be a range
            if aa.include?(c.ord)
              true
            else
              aa.max < c.ord ? within_set?(set[1..-1], c) : false
            end
          end
        end
      end
    end

  end

end
