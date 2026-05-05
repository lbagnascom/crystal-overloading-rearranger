class Foo
  @[Slot]
  def bar()
  end

  @[Slot]
  def bar(a = true)
  end
end

Foo.new.bar false
