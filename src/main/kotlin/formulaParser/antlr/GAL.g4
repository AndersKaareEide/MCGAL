grammar GAL;


// Parser rules
formula : form EOF;
form : prop=PROP                        #atomicForm
    | op=NEG inner=form                 #negForm
    | left=form op=CONJ right=form      #conjForm
    | left=form op=DISJ right=form      #disjForm
    | left=form op=IMPL right=form      #implForm
    | 'K' agent=AGENT '('inner=form')'   #knowsForm
    | '(' inner=form ')'                #parensForm
    | '['announced=form']' inner=form   #announceForm
    | agents inner=form                 #groupannForm
    ;

agents : '['AGENT(COMMA AGENT)*']';

// Lexer rules
WHITESPACE : ' ' -> skip;

AGENT   : [A-ZÆØÅ][a-zæøå]*([0-1])*;
PROP    : [a-zæøå]+([0-9])*; //Note: PROP is also used for agents
COMMA   : ',';
NEG     : '!';
CONJ    : '&';
DISJ    : '|';
IMPL    : '->';
