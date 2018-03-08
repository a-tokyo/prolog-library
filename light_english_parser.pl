% % ########################
% %  DEFS LANG
% % ########################

% % ADJECTIVES
adjective(adjective(young)) --> [young].
adjective(adjective(big)) --> [big].
adjective(adjective(large)) --> [large].
adjective(adjective(empty)) --> [empty].
adjective(adjective(old)) --> [old].
adjective(adjective(poor)) --> [poor].
adjective(adjective(white)) --> [white].
adjective(adjective(brilliant)) --> [brilliant].
adjective(adjective(talented)) --> [talented].
adjective(adjective(bright)) --> [bright].
adjective(adjective(pretty)) --> [pretty].
adjective(adjective(small)) --> [small].
adjective(adjective(big)) --> [big].
adjective(adjective(fat)) --> [fat].
adjective(adjective(smart)) --> [smart].
adjective(adjective(intelligent)) --> [intelligent].
adjective(adjective(sophisticated)) --> [sophisticated].
adjective(adjective(amazing)) --> [amazing].
adjective(adjective(tall)) --> [tall].
adjective(adjective(short)) --> [short].
adjective(adjective(huge)) --> [huge].
adjective(adjective(amazing)) --> [amazing].
adjective(adjective(tiny)) --> [tiny].
adjective(adjective(long)) --> [long].
adjective(adjective(beautiful)) --> [beautiful].
% CONJUNCTIVES
conjunctive(conjunctive(and)) --> [and].
conjunctive(conjunctive(also)) --> [also].
conjunctive(conjunctive(however)) --> [however].
conjunctive(conjunctive(nevertheless)) --> [nevertheless].
conjunctive(conjunctive(although)) --> [although].
conjunctive(conjunctive(regardless)) --> [regardless].
% % ADVERBS
adverb(adverb(quickly)) --> [quickly].
adverb(adverb(slowly)) --> [slowly].
adverb(adverb(abruptly)) --> [abruptly].
adverb(adverb(firmly)) --> [firmly].
adverb(adverb(delightfully)) --> [delightfully].
adverb(adverb(lightfully)) --> [lightfully].
adverb(adverb(delicately)) --> [delicately].
adverb(adverb(wearily)) --> [wearily].
adverb(adverb(beautifully)) --> [beautifully].
adverb(adverb(randomly)) --> [randomly].
adverb(adverb(willingly)) --> [willingly].
adverb(adverb(wickedly)) --> [wickedly].
adverb(adverb(sloppily)) --> [sloppily].
% % DETERMINANTS
determiner(determiner(the)) --> [the].
determiner(determiner(a)) --> [a].
determiner(determiner(every)) --> [every].
determiner(determiner(some)) --> [some].
determiner(determiner(any)) --> [any].
determiner(determiner(many)) --> [many].
determiner(determiner(an)) --> [an].
determiner(determiner(all)) --> [all].
determiner(determiner(more)) --> [more].
determiner(determiner(less)) --> [less].
% % NOUNS
noun(noun(boy)) --> [boy].
noun(noun(box)) --> [box].
noun(noun(room)) --> [room].
noun(noun(school)) --> [school].
noun(noun(woman)) --> [woman].
noun(noun(man)) --> [man].
noun(noun(envelope)) --> [envelope].
noun(noun(shed)) --> [shed].
noun(noun(building)) --> [building].
noun(noun(tree)) --> [tree].
noun(noun(girl)) --> [girl].
noun(noun(students)) --> [students].
noun(noun(professors)) --> [professors].
noun(noun(lecturers)) --> [lecturers].
noun(noun(scientists)) --> [scientists].
noun(noun(researchers)) --> [researchers].
noun(noun(worker)) --> [worker].
noun(noun(workers)) --> [workers].
noun(noun(guy)) --> [guy].
noun(noun(human)) --> [human].
noun(noun(person)) --> [person].
noun(noun(university)) --> [university].
noun(noun(ministry)) --> [ministry].
noun(noun(r2d2)) --> [r2d2].
noun(noun(empire)) --> [empire].
noun(noun(luke)) --> [luke].
noun(noun(vader)) --> [vader].
noun(noun(star)) --> [star].
noun(noun(task)) --> [task].
% % PREPOSITIONS
preposition(preposition(in)) --> [in].
preposition(preposition(after)) --> [after].
preposition(preposition(behind)) --> [behind].
preposition(preposition(on)) --> [on].
preposition(preposition(before)) --> [before].
preposition(preposition(beside)) --> [beside].
preposition(preposition(by)) --> [by].
preposition(preposition(at)) --> [at].
preposition(preposition(under)) --> [under].
preposition(preposition(below)) --> [below].
preposition(preposition(towards)) --> [towards].
preposition(preposition(and)) --> [and].
% % VERBS
verb(verb(push)) --> [pushed].
verb(verb(store)) --> [stored].
verb(verb(give)) --> [gave].
verb(verb(climb)) --> [climbed].
verb(verb(watch)) --> [watched].
verb(verb(admire)) --> [admired].
verb(verb(appreciate)) --> [appreciated].
verb(verb(eat)) --> [ate].
verb(verb(see)) --> [saw].
verb(verb(gift)) --> [gifted].
verb(verb(drink)) --> [drank].
verb(verb(ascend)) --> [ascended].
verb(verb(make)) --> [made].
verb(verb(play)) --> [played].
verb(verb(walk)) --> [walked].
verb(verb(move)) --> [moved].
verb(verb(pull)) --> [pulled].
verb(verb(work)) --> [worked].
verb(verb(do)) --> [did].
verb(verb(become)) --> [became].
verb(verb(ask)) --> [asked].
verb(verb(feel)) --> [felt].
verb(verb(put)) --> [put].
verb(verb(love)) --> [loved].
verb(verb(build)) --> [built].
verb(verb(destroy)) --> [destroyed].
verb(verb(be)) --> [was].
verb(verb(have)) --> [had].
verb(verb(try)) --> [tried].
verb(verb(accomplish)) --> [accomplish].

% % ########################
% %  LOGIC
% % ########################


%% Permutation generation, bulding block

% % Sentence
% sentence --> noun_phrase, adv_phrase.
% sentence --> noun_phrase, verb_phrase.


% % Noun Phrase
% noun_phrase --> det, noun.
% noun_phrase --> det, adj_phrase.
% % noun_phrase --> noun_phrase, cnj, noun_phrase.


% % Adjective Phrase
% adj_phrase --> adj, noun.

% % Verb Phrase
% verb_phrase --> verb, noun_phrase.
% % verb_phrase --> adv, noun_phrase.
% % Adverb Phrase
% adv_phrase --> adv, verb_phrase.


% % Preposition Phrase
% prep_phrase --> prep, noun_phrase.

% Appending s start variable for submission purposes
s(s(X)) --> sentence(X).

% Sentence
sentence(sentence(X)) --> regular_sentence(X) | conjuctive_sentence(X).
regular_sentence(regular_sentence(X,Y)) --> noun_phrase(X), verb_phrase(Y).
conjuctive_sentence(conjuctive_sentence(X,Y,Z)) --> regular_sentence(X), conjunctive(Y), sentence(Z).

% Noun
% noun_phrase(noun_phrase(X,Y)) --> determiner(X), noun(Y).
% noun_phrase(noun_phrase(X,Y,Z)) --> noun_phrase1(X,Y,Z).
% noun_phrase(noun_phrase(X,Y,Z)) --> noun_phrase(X), conjunctive(Y), noun_phrase(Z).

noun_phrase(noun_phrase(X)) -->
  (noun_phrase1(X)) | (noun_phrase2(X)).
noun_phrase(noun_phrase(X,Y)) -->
  (noun_phrase1(X),noun_phrase(Y)) | (noun_phrase2(X),noun_phrase(Y)).
noun_phrase(noun_phrase(X,Y,Z)) -->
  (noun_phrase1(X), preposition(Y), noun_phrase2(Z)) | (noun_phrase1(X), preposition(Y), noun_phrase(Z)).
 
noun_phrase1(noun_phrase1(X,Y)) -->
  determiner(X), noun(Y).
noun_phrase1(noun_phrase1(X,Y,Z)) -->
  determiner(X), adjective_phrase(Y), noun(Z).

noun_phrase2(noun_phrase2(X)) -->
  noun(X).
noun_phrase2(noun_phrase2(X,Y)) -->
  adjective_phrase(X), noun(Y) | determiner(X), noun(Y).
noun_phrase2(noun_phrase2(W,X,Y,Z)) -->
  adjective_phrase(W), noun(X), conjunctive(Y), noun_phrase2(Z).

% Verb
verb_phrase(verb_phrase(X,Y)) -->
  verb_phrase1(X),noun_phrase(Y).
verb_phrase(verb_phrase(W,X,Y,Z)) -->
  verb_phrase1(W), noun_phrase(X), conjunctive(Y), verb_phrase(Z) | verb_phrase1(W), noun_phrase(X), preposition(Y), noun_phrase(Z).
verb_phrase1(verb_phrase1(X)) -->
  verb(X).
verb_phrase1(verb_phrase1(X,Y)) -->
  adverb_phrase(X), verb_phrase1(Y).
verb_phrase1(verb_phrase1(X,Y,Z)) -->
    verb(X),conjunctive(Y),verb_phrase1(Z).

% Adjective
adjective_phrase(adjective_phrase(X)) -->
  adjective(X).
adjective_phrase(adjective_phrase(X,Y)) -->
  adjective(X), adjective_phrase(Y).

% Adverb
adverb_phrase(adverb_phrase(X)) -->
  adverb(X).
adverb_phrase(adverb_phrase(X)) -->
  adverb(X), adverb_phrase(X).

% ########################
%  END OF FILE
% ########################