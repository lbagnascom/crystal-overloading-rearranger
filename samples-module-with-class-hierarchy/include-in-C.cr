module M
end

class A
end

class B < A
end

class C < B
  include M
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
