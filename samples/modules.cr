module M1
end

module M2
end

class C
	include M1
	include M2
end

@[Slot]
def f(x : M1)
  "M1"
end

@[Slot]
def f(x : M2)
  "M2"
end

pp f(C.new)
