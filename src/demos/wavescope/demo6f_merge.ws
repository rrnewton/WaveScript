
// Simple test of merge primitive.
// It just merges two streams (no tagging them with numbers or anything).

// It should be implemented by unionList + some optimizations, or
// vice-versa, but it's currently handled separately (unfortunately).

s1 = iterate _ in timer(30.0){
  state{ c :: Int64 = 0 }
  c += 1;
  emit c
}

s2 = iterate _ in timer(40.0){
  state{ c=100 }
  c += 1;
  emit c
}

main = merge(s1,s2)
