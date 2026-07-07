module M
end

class A
end

class B < A
  include M
end

class C < B
end

@[Slot]
def f(x : M)
	"Module M"
end

@[Slot]
def f(x : B)
	"Class B"
end

@[Slot]
def f(x : A)
	"Class A"
end

pp f(C.new)
