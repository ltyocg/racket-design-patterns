import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.Vocabulary;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;

public final class ParseJava {
    private ParseJava() {
    }

    public static void main(String[] args) throws Exception {
        boolean includeHidden = false;
        String inputArg = null;

        for (String arg : args) {
            if (arg.equals("--include-hidden")) {
                includeHidden = true;
            } else if (inputArg == null) {
                inputArg = arg;
            } else {
                usage();
            }
        }

        if (inputArg == null) {
            usage();
        }

        CharStream input = inputArg.equals("--stdin")
            ? CharStreams.fromString(readAllStdin())
            : CharStreams.fromFileName(inputArg);

        JavaLexer lexer = new JavaLexer(input);
        CommonTokenStream tokenStream = new CommonTokenStream(lexer);
        JavaParser parser = new JavaParser(tokenStream);
        ThrowingErrorListener errorListener = new ThrowingErrorListener();
        lexer.removeErrorListeners();
        parser.removeErrorListeners();
        lexer.addErrorListener(errorListener);
        parser.addErrorListener(errorListener);

        ParseTree tree = parser.compilationunit();
        tokenStream.fill();
        String[] ruleNames = parser.getRuleNames();
        Vocabulary vocabulary = parser.getVocabulary();
        StringBuilder out = new StringBuilder();
        appendTree(out, tree, ruleNames, vocabulary, tokenStream, includeHidden, new HashSet<Integer>());
        System.out.println(out);
    }

    private static void usage() {
        System.err.println("usage: ParseJava [--include-hidden] (<path> | --stdin)");
        System.exit(2);
    }

    private static String readAllStdin() throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        byte[] chunk = new byte[8192];
        int read;
        while ((read = System.in.read(chunk)) != -1) {
            buffer.write(chunk, 0, read);
        }
        return buffer.toString(StandardCharsets.UTF_8.name());
    }

    private static void appendTree(
        StringBuilder out,
        ParseTree tree,
        String[] ruleNames,
        Vocabulary vocabulary,
        CommonTokenStream tokenStream,
        boolean includeHidden,
        Set<Integer> emittedHiddenTokenIndexes
    ) {
        if (tree instanceof TerminalNode) {
            Token token = ((TerminalNode) tree).getSymbol();
            appendToken(out, token, vocabulary, includeHidden);
            return;
        }

        if (tree instanceof RuleNode) {
            RuleNode ruleNode = (RuleNode) tree;
            int ruleIndex = ruleNode.getRuleContext().getRuleIndex();
            out.append('(');
            appendSymbol(out, ruleNames[ruleIndex]);
            for (int i = 0; i < tree.getChildCount(); i++) {
                if (includeHidden) {
                    appendHiddenTokensBeforeChild(out, tokenStream, tree.getChild(i), vocabulary, emittedHiddenTokenIndexes);
                }
                out.append(' ');
                appendTree(out, tree.getChild(i), ruleNames, vocabulary, tokenStream, includeHidden, emittedHiddenTokenIndexes);
            }
            out.append(')');
            return;
        }

        appendString(out, tree.getText());
    }

    private static void appendHiddenTokensBeforeChild(
        StringBuilder out,
        CommonTokenStream tokenStream,
        ParseTree child,
        Vocabulary vocabulary,
        Set<Integer> emittedHiddenTokenIndexes
    ) {
        Token token = firstToken(child);
        if (token == null) {
            return;
        }

        List<Token> hiddenTokens = tokenStream.getHiddenTokensToLeft(token.getTokenIndex());
        if (hiddenTokens == null) {
            return;
        }

        for (Token hiddenToken : hiddenTokens) {
            if (emittedHiddenTokenIndexes.add(hiddenToken.getTokenIndex())) {
                out.append(' ');
                appendToken(out, hiddenToken, vocabulary, true);
            }
        }
    }

    private static Token firstToken(ParseTree tree) {
        if (tree instanceof TerminalNode) {
            return ((TerminalNode) tree).getSymbol();
        }
        if (tree instanceof RuleNode) {
            RuleNode ruleNode = (RuleNode) tree;
            if (ruleNode.getRuleContext() instanceof ParserRuleContext) {
                return ((ParserRuleContext) ruleNode.getRuleContext()).getStart();
            }
        }
        return null;
    }

    private static void appendToken(
        StringBuilder out,
        Token token,
        Vocabulary vocabulary,
        boolean includeChannel
    ) {
        int type = token.getType();
        String name = type == Token.EOF ? "EOF" : vocabulary.getSymbolicName(type);
        out.append("(token ");
        appendSymbol(out, name == null ? Integer.toString(type) : name);
        out.append(' ');
        appendString(out, token.getText());
        out.append(' ');
        out.append(token.getLine());
        out.append(' ');
        out.append(token.getCharPositionInLine());
        out.append(' ');
        out.append(token.getStartIndex());
        out.append(' ');
        out.append(token.getStopIndex());
        if (includeChannel) {
            out.append(' ');
            appendSymbol(out, tokenChannelName(token.getChannel()));
        }
        out.append(')');
    }

    private static String tokenChannelName(int channel) {
        if (channel == Token.DEFAULT_CHANNEL) {
            return "DEFAULT";
        }
        if (channel == Token.HIDDEN_CHANNEL) {
            return "HIDDEN";
        }
        return "CHANNEL_" + channel;
    }

    private static void appendSymbol(StringBuilder out, String value) {
        out.append('|');
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            if (ch == '|' || ch == '\\') {
                out.append('\\');
            }
            out.append(ch);
        }
        out.append('|');
    }

    private static void appendString(StringBuilder out, String value) {
        out.append('"');
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            switch (ch) {
                case '\\':
                    out.append("\\\\");
                    break;
                case '"':
                    out.append("\\\"");
                    break;
                case '\n':
                    out.append("\\n");
                    break;
                case '\r':
                    out.append("\\r");
                    break;
                case '\t':
                    out.append("\\t");
                    break;
                default:
                    if (ch < 0x20) {
                        out.append(String.format("\\u%04x", (int) ch));
                    } else {
                        out.append(ch);
                    }
                    break;
            }
        }
        out.append('"');
    }

    private static final class ThrowingErrorListener extends BaseErrorListener {
        @Override
        public void syntaxError(
            Recognizer<?, ?> recognizer,
            Object offendingSymbol,
            int line,
            int charPositionInLine,
            String msg,
            RecognitionException e
        ) {
            throw new IllegalArgumentException("Parse error at line " + line + ", col " + charPositionInLine + ": " + msg, e);
        }
    }
}
