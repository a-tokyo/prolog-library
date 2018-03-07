% ########################
%  DEFS
% ########################

% DETERMINANTS
det --> [the].
det --> [a].
det --> [every].
det --> [some].
det --> [any].
det --> [many].
det --> [few].
det --> [an].
det -> [all]
det -> [more]
det -> [less]


% PREPOSITIONS
prep --> [in]
prep --> [after]
prep --> [behind]

prep --> [on]
prep --> [before]
prep --> [infront of]
prep --> [beside]
prep --> [by]
prep --> [at]
prep --> [under]
prep --> [below]
prep --> [towards]


% NOUNS
noun --> [boy].
noun --> [box].
noun --> [room].
noun --> [school].

noun --> [woman].
noun --> [man].
noun --> [envelope].
noun --> [shed].
noun --> [building].

noun --> [tree].
noun --> [girl].

noun --> [students].
noun --> [professors].
noun --> [lecturers].
noun --> [scientists].
noun --> [researchers].

noun --> [worker].
noun --> [workers].
noun --> [guy].
noun --> [human].
noun --> [person].
noun --> [university].
noun --> [ministry].

% ADJECTIVES
adj --> [young].
adj --> [big].
adj --> [large].
adj --> [empty].

adj --> [old].
adj --> [poor].
adj --> [white].

adj --> [brilliant].
adj --> [talented].
adj --> [bright].

% adj --> [small].
% adj --> [big].
% adj --> [fat].
% adj --> [smart].
% adj --> [intelligent].
% adj --> [sophisticated].
% adj --> [amazing].
% adj --> [tall].
% adj --> [short].
% adj --> [huge].
% adj --> [tiny].
% adj --> [long].


% ADVERBS
adv --> [quickly].

adv -> [slowly].
adv -> [abruptly].
adv -> [firmly].
adv -> [delightfully].
adv -> [lightfully].
adv -> [delicately].
adv -> [wearily].
adv -> [beautifully].
adv -> [randomly].
adv -> [willingly].
adv -> [wickedly].
adv -> [sloppily].

% VERBS
verb --> [pushed].
verb --> [stored].
verb --> [gave].
verb --> [climbed].
verb --> [watched].
verb --> [admired].
verb --> [appreciated].

verb --> [ate].
verb --> [saw].
verb --> [gifted].
verb --> [drank].
verb --> [ascended].
verb --> [made].
verb --> [played].
verb --> [moved].
verb --> [pulled].
verb --> [worked].
verb --> [did].
verb --> [became].
verb --> [asked].
verb --> [felt].
verb --> [became].


% ########################
%  LOGIC
% ########################


sentence --> noun_phrase, verb_phrase.
noun_phrase --> det, noun.
verb_phrase --> verb, noun_phrase.
















% ########################
%  END OF FILE
% ########################