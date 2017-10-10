// Generated from C:/Users/AndersLaptop/Workspace/HaskellProjects/Masterprosjekt/src/main/kotlin/formulaParser/antlr\GAL.g4 by ANTLR 4.7
package formulaParser.antlr;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class GALParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, WHITESPACE=6, AGENT=7, PROP=8, 
		COMMA=9, NEG=10, CONJ=11, DISJ=12, IMPL=13;
	public static final int
		RULE_formula = 0, RULE_form = 1, RULE_agents = 2;
	public static final String[] ruleNames = {
		"formula", "form", "agents"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'K'", "'('", "')'", "'['", "']'", "' '", null, null, "','", "'!'", 
		"'&'", "'|'", "'->'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, "WHITESPACE", "AGENT", "PROP", "COMMA", 
		"NEG", "CONJ", "DISJ", "IMPL"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "GAL.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public GALParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class FormulaContext extends ParserRuleContext {
		public FormContext form() {
			return getRuleContext(FormContext.class,0);
		}
		public TerminalNode EOF() { return getToken(GALParser.EOF, 0); }
		public FormulaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_formula; }
	}

	public final FormulaContext formula() throws RecognitionException {
		FormulaContext _localctx = new FormulaContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_formula);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(6);
			form(0);
			setState(7);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FormContext extends ParserRuleContext {
		public FormContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_form; }
	 
		public FormContext() { }
		public void copyFrom(FormContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class ImplFormContext extends FormContext {
		public FormContext left;
		public Token op;
		public FormContext right;
		public List<FormContext> form() {
			return getRuleContexts(FormContext.class);
		}
		public FormContext form(int i) {
			return getRuleContext(FormContext.class,i);
		}
		public TerminalNode IMPL() { return getToken(GALParser.IMPL, 0); }
		public ImplFormContext(FormContext ctx) { copyFrom(ctx); }
	}
	public static class GroupannFormContext extends FormContext {
		public FormContext inner;
		public AgentsContext agents() {
			return getRuleContext(AgentsContext.class,0);
		}
		public FormContext form() {
			return getRuleContext(FormContext.class,0);
		}
		public GroupannFormContext(FormContext ctx) { copyFrom(ctx); }
	}
	public static class NegFormContext extends FormContext {
		public Token op;
		public FormContext inner;
		public TerminalNode NEG() { return getToken(GALParser.NEG, 0); }
		public FormContext form() {
			return getRuleContext(FormContext.class,0);
		}
		public NegFormContext(FormContext ctx) { copyFrom(ctx); }
	}
	public static class ConjFormContext extends FormContext {
		public FormContext left;
		public Token op;
		public FormContext right;
		public List<FormContext> form() {
			return getRuleContexts(FormContext.class);
		}
		public FormContext form(int i) {
			return getRuleContext(FormContext.class,i);
		}
		public TerminalNode CONJ() { return getToken(GALParser.CONJ, 0); }
		public ConjFormContext(FormContext ctx) { copyFrom(ctx); }
	}
	public static class DisjFormContext extends FormContext {
		public FormContext left;
		public Token op;
		public FormContext right;
		public List<FormContext> form() {
			return getRuleContexts(FormContext.class);
		}
		public FormContext form(int i) {
			return getRuleContext(FormContext.class,i);
		}
		public TerminalNode DISJ() { return getToken(GALParser.DISJ, 0); }
		public DisjFormContext(FormContext ctx) { copyFrom(ctx); }
	}
	public static class KnowsFormContext extends FormContext {
		public Token agent;
		public FormContext inner;
		public TerminalNode AGENT() { return getToken(GALParser.AGENT, 0); }
		public FormContext form() {
			return getRuleContext(FormContext.class,0);
		}
		public KnowsFormContext(FormContext ctx) { copyFrom(ctx); }
	}
	public static class ParensFormContext extends FormContext {
		public FormContext inner;
		public FormContext form() {
			return getRuleContext(FormContext.class,0);
		}
		public ParensFormContext(FormContext ctx) { copyFrom(ctx); }
	}
	public static class AnnounceFormContext extends FormContext {
		public FormContext announced;
		public FormContext inner;
		public List<FormContext> form() {
			return getRuleContexts(FormContext.class);
		}
		public FormContext form(int i) {
			return getRuleContext(FormContext.class,i);
		}
		public AnnounceFormContext(FormContext ctx) { copyFrom(ctx); }
	}
	public static class AtomicFormContext extends FormContext {
		public Token prop;
		public TerminalNode PROP() { return getToken(GALParser.PROP, 0); }
		public AtomicFormContext(FormContext ctx) { copyFrom(ctx); }
	}

	public final FormContext form() throws RecognitionException {
		return form(0);
	}

	private FormContext form(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		FormContext _localctx = new FormContext(_ctx, _parentState);
		FormContext _prevctx = _localctx;
		int _startState = 2;
		enterRecursionRule(_localctx, 2, RULE_form, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(33);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,0,_ctx) ) {
			case 1:
				{
				_localctx = new AtomicFormContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;

				setState(10);
				((AtomicFormContext)_localctx).prop = match(PROP);
				}
				break;
			case 2:
				{
				_localctx = new NegFormContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(11);
				((NegFormContext)_localctx).op = match(NEG);
				setState(12);
				((NegFormContext)_localctx).inner = form(8);
				}
				break;
			case 3:
				{
				_localctx = new KnowsFormContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(13);
				match(T__0);
				setState(14);
				((KnowsFormContext)_localctx).agent = match(AGENT);
				setState(15);
				match(T__1);
				setState(16);
				((KnowsFormContext)_localctx).inner = form(0);
				setState(17);
				match(T__2);
				}
				break;
			case 4:
				{
				_localctx = new ParensFormContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(19);
				match(T__1);
				setState(20);
				((ParensFormContext)_localctx).inner = form(0);
				setState(21);
				match(T__2);
				}
				break;
			case 5:
				{
				_localctx = new AnnounceFormContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(23);
				match(T__3);
				setState(24);
				((AnnounceFormContext)_localctx).announced = form(0);
				setState(25);
				match(T__4);
				setState(26);
				((AnnounceFormContext)_localctx).inner = form(2);
				}
				break;
			case 6:
				{
				_localctx = new GroupannFormContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(28);
				match(T__3);
				setState(29);
				agents();
				setState(30);
				match(T__4);
				setState(31);
				((GroupannFormContext)_localctx).inner = form(1);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(46);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(44);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
					case 1:
						{
						_localctx = new ConjFormContext(new FormContext(_parentctx, _parentState));
						((ConjFormContext)_localctx).left = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_form);
						setState(35);
						if (!(precpred(_ctx, 7))) throw new FailedPredicateException(this, "precpred(_ctx, 7)");
						setState(36);
						((ConjFormContext)_localctx).op = match(CONJ);
						setState(37);
						((ConjFormContext)_localctx).right = form(8);
						}
						break;
					case 2:
						{
						_localctx = new DisjFormContext(new FormContext(_parentctx, _parentState));
						((DisjFormContext)_localctx).left = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_form);
						setState(38);
						if (!(precpred(_ctx, 6))) throw new FailedPredicateException(this, "precpred(_ctx, 6)");
						setState(39);
						((DisjFormContext)_localctx).op = match(DISJ);
						setState(40);
						((DisjFormContext)_localctx).right = form(7);
						}
						break;
					case 3:
						{
						_localctx = new ImplFormContext(new FormContext(_parentctx, _parentState));
						((ImplFormContext)_localctx).left = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_form);
						setState(41);
						if (!(precpred(_ctx, 5))) throw new FailedPredicateException(this, "precpred(_ctx, 5)");
						setState(42);
						((ImplFormContext)_localctx).op = match(IMPL);
						setState(43);
						((ImplFormContext)_localctx).right = form(6);
						}
						break;
					}
					} 
				}
				setState(48);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class AgentsContext extends ParserRuleContext {
		public List<TerminalNode> AGENT() { return getTokens(GALParser.AGENT); }
		public TerminalNode AGENT(int i) {
			return getToken(GALParser.AGENT, i);
		}
		public List<TerminalNode> COMMA() { return getTokens(GALParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(GALParser.COMMA, i);
		}
		public AgentsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_agents; }
	}

	public final AgentsContext agents() throws RecognitionException {
		AgentsContext _localctx = new AgentsContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_agents);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(49);
			match(AGENT);
			setState(54);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(50);
				match(COMMA);
				setState(51);
				match(AGENT);
				}
				}
				setState(56);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 1:
			return form_sempred((FormContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean form_sempred(FormContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 7);
		case 1:
			return precpred(_ctx, 6);
		case 2:
			return precpred(_ctx, 5);
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\17<\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3$\n\3\3\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\3\3\7\3/\n\3\f\3\16\3\62\13\3\3\4\3\4\3\4\7\4\67"+
		"\n\4\f\4\16\4:\13\4\3\4\2\3\4\5\2\4\6\2\2\2A\2\b\3\2\2\2\4#\3\2\2\2\6"+
		"\63\3\2\2\2\b\t\5\4\3\2\t\n\7\2\2\3\n\3\3\2\2\2\13\f\b\3\1\2\f$\7\n\2"+
		"\2\r\16\7\f\2\2\16$\5\4\3\n\17\20\7\3\2\2\20\21\7\t\2\2\21\22\7\4\2\2"+
		"\22\23\5\4\3\2\23\24\7\5\2\2\24$\3\2\2\2\25\26\7\4\2\2\26\27\5\4\3\2\27"+
		"\30\7\5\2\2\30$\3\2\2\2\31\32\7\6\2\2\32\33\5\4\3\2\33\34\7\7\2\2\34\35"+
		"\5\4\3\4\35$\3\2\2\2\36\37\7\6\2\2\37 \5\6\4\2 !\7\7\2\2!\"\5\4\3\3\""+
		"$\3\2\2\2#\13\3\2\2\2#\r\3\2\2\2#\17\3\2\2\2#\25\3\2\2\2#\31\3\2\2\2#"+
		"\36\3\2\2\2$\60\3\2\2\2%&\f\t\2\2&\'\7\r\2\2\'/\5\4\3\n()\f\b\2\2)*\7"+
		"\16\2\2*/\5\4\3\t+,\f\7\2\2,-\7\17\2\2-/\5\4\3\b.%\3\2\2\2.(\3\2\2\2."+
		"+\3\2\2\2/\62\3\2\2\2\60.\3\2\2\2\60\61\3\2\2\2\61\5\3\2\2\2\62\60\3\2"+
		"\2\2\638\7\t\2\2\64\65\7\13\2\2\65\67\7\t\2\2\66\64\3\2\2\2\67:\3\2\2"+
		"\28\66\3\2\2\289\3\2\2\29\7\3\2\2\2:8\3\2\2\2\6#.\608";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}