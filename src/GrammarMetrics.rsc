module GrammarMetrics

import ParseTree;
import GenericMetrics;
import Set;

public int grammarCyclomaticComplexity(Tree tree) {
  // For grammars, it is just counting decisions.
  // See Power/Malloy 2004
  return numOfGrammarDecisionNodes(tree);
}

public int numOfNonTerminals(Tree tree) {
  return size(nonTerminals(tree));
}

public int numOfTerminals(Tree tree) {
  return size(terminals(tree));
}

public int avgRHSSize(Tree tree) {
  totalRHSSize = 0;
  top-down-break visit (tree) {
    case t:appl(prod(_, _, attrs([_*, term(class("Rule")), _*])), _): 
      visit (t) {
        case appl(prod(_, _, attrs([_*, term(class("NonTerminal")), _*])), _): 
	  totalRHSSize += 1;
        case appl(prod(_, _, attrs([_*, term(class("Terminal")), _*])), _): 
 	  totalRHSSize += 1;
      }
  }
  return totalRHSSize / size(nonTerminals(tree));
}

public set[Tree] extractRules(Tree tree) {
   set[Tree] rules = {};
   top-down-break visit(tree) {
     case t:appl(prod(_, _, attrs([_*, term(class("Rule")), _*])), _): 
        rules += {t};
   }
   return rules;
}


// does not work: { t | /t:appl(prod(_, _, attrs([_*, term(class("NonTerminal")), _*])), _) <- tree };
public set[Tree] nonTerminals(Tree tree) {
  nts = {};
  visit (tree) {
    case t:appl(prod(_, _, attrs([_*, term(class("NonTerminal")), _*])), _): 
      nts += {t};
  }
  return nts;
}

@doc{Assumes the first argument of Rule annotated prod is the defined symbol}
public rel[str, str] dependencies(Tree tree) {
  result = {};
  visit (tree) {
    case t:appl(prod(_, _, attrs([_*, term(class("Rule")), _*])), [Tree sym, _*]): {
      d = "<sym>";
      for (nt <- nonTerminals(t))
         result += {<d, "<nt>">};
    }
  }
  return result;
}

public set[Tree] terminals(Tree tree) {
  ts = {};
  visit (tree) {
    case t:appl(prod(_, _, attrs([_*, term(class("Terminal")), _*])), _): 
      ts += {t};
  }
  return ts;
}
