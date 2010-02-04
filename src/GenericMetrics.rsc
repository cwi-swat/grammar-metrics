module GenericMetrics

import ToString;
import ParseTree;
import List;

@doc{
This function counts the number of nodes in a parse tree that
have been annotated with class("Decision"). Used for computing
the McCabe cyclomatic complexity metric.
}

public int numOfDecisionNodes(Tree tree) {
  return size(nodesWithClass(tree, "Decision"));
}

public int numOfGrammarDecisionNodes(Tree tree) {
  return size(nodesWithClass(tree, "GrammarDecision"));
}


public list[Tree] nodesWithClass(Tree tree, str c) {
  list[Tree] result = [];
  visit (tree) {
    case t:appl(prod(_, _, attrs([_*, term(class(c)), _*])), _): 
      result += [t];
  }
  return result;
}



@doc{
This function generically computes non-comment lines of code (NCLOC)
of arbitrary parse trees. It assumes that whitespace layout nodes
are labeled with the production attribute cons("whitespace"); as
in basic/Whitespace. NCLOC calculation proceeds by counting newlines in
(non-Comment) layout nodes. Multiple newlines in a single layout node 
are counted as one.

Examples:
  nonCommentLinesOfCode(parse(#S, "a b")) -> 1
  nonCommentLinesOfCode(parse(#S, "a \n b")) -> 2
  nonCommentLinesOfCode(parse(#S, "a \n\n\n\n b")) -> 2
  nonCommentLinesOfCode(parse(#S, "a /* \n\n */ \n\n\n\n b")) -> 2
}
public int nonCommentLinesOfCode(Tree tree) {
  int count = 1;
  visit (tree) {
    case t:appl(prod([cf(layout())],cf(opt(layout())), _), _): {
       // If we find a layout node
       nl = false;
       visit (t) {
          // ...we visit all whitespace nodes under it (excluding comments)
          case  ws:appl(prod(_, _, attrs([term(cons("whitespace"))])), _):
            // ...and if it contains one ore more newlines
            if (/\n/ := toString(ws)) nl = true;
       }
       // ...increase count.
       if (nl) count += 1;
    }
  }
  return count;
}



