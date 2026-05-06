@[Slot]
def foo(x : T, y : S) forall T, S
  "x : T, y : S"
end

@[Slot]
def foo(x : _, y : _)
  "x : _, y : _"
end

pp foo(true, false)
