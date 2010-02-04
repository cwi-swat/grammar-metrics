module Diff

import String;
import Set;
import Integer;

public rel[str,str] pairs(str s) {
   z = { x | /<x:.>/ <- toUpperCase(s) };
   return { <c1, c2> | c1 <- z, c2 <- z, /<c1><c2>/i := s };
}

public real nameSimilarity(str s1, str s2) {
  pairs1 = pairs(s1);
  pairs2 = pairs(s2);
  int union = size(pairs1) + size(pairs2);
  isect = pairs1 & pairs2;
  if (union == 0) return 0.0;
  return (size(isect) * 2.0) / union;   
}

test nameSimilarity("DrawVerticalAction", "VerticalDrawAction") > 
   nameSimilarity("AddVerticalAction", "VerticalDrawAction");

private int pow = 1; // TODO: should be a dynvar.

public real power(real n, int e) {
  result = n;
  while (e > 1) {
   result *= n;
   e -= 1;
  }
  return result;
}

alias Relation = rel[str from, str to, int count];

public real structureSimilarity(str e1, str e2, Relation facts) {
  graph = facts<from, to>;
  e_of_r1 = graph[e1];
  e_of_r2 = graph[e2];
  if (e_of_r1 == {} && e_of_r2 == {}) {
    pow += 1;
    return power(nameSimilarity(e1, e2), pow);
  }

  beforeCount = 0; afterCount = 0;
  for (er1 <- e_of_r1, er2 <- e_of_r2) {
     if (er1 == er2) {
        beforeCount += facts[e1, er1];
        afterCount += facts[e2, er2];
        e_of_r1 -= {er1};
        e_of_r2 -= {er2};
     }
   }

  beforeLeftCount = 0; afterLeftCount = 0;
  for (er1 <- e_of_r1)
    beforeLeftCount += facts[e1, er1];
  for (er2 <- e_of_r2)
    afterLeftCount += facts[e2, er2];

  int mi = min(beforeCount, afterCount);
  int ma = max(beforeCount, afterCount);

  return mi * (1.0 / max + beforeLeftCount + afterLeftCount);
}

public real computeSimilarityMetric(str e1, str e2, set[Relation] relations, int n) {
  nameSimilarity = nameSimilarity(e1, e2);
  /*global*/ pow = 0;
  metric = 0.0;
  for (r <- relations)
    metrics += structureSimilarity(e1, e2, r);
  return (nameSimilarity + metric) / (nameSimilarity + n); 
}

alias Queue[&T] = list[&T];

// TODO: adt for "edits"
public void umlDiff() {
   Queue next, match, results;
   int level = VIRTUAL_ROOT;
   
}

