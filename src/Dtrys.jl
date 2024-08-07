module Dtrys

using Moshi
using Moshi.Data: @data
using Moshi.Match: @match
using AssociatedTests, Test
import Base: tail

abstract type Path end

struct VectorPath <: Path
  segments::Vector{Symbol}
end

function Path(segments::Vector)
  VectorPath(segments)
end

function Base.length(p::VectorPath)
  length(p.segments)
end

Base.isempty(p::VectorPath) = isempty(p.segments)

head(p::VectorPath) = p.segments[1]

tail(p::VectorPath) = Subpath(p, 2)

struct Subpath <: Path
  path::VectorPath
  starting_from::Int
end

Base.isempty(p::Subpath) = p.starting_from > length(p.path)

head(p::Subpath) = p.path.segments[p.starting_from]

tail(p::Subpath) = Subpath(p.path, p.starting_from + 1)

@data NonEmptyDtry{A} begin
  Leaf(Ref{A})
  # invariant: this must be sorted
  Node(Vector{Pair{Symbol,NonEmptyDtry.Type{A}}})
end

using .NonEmptyDtry: Leaf, Node, Type as NED

function leaf(x::A) where {A}
  Leaf(Ref(x))
end

function node(pairs...)
  pairs = [pairs...]
  sort!(pairs, by=first)
  Node(pairs)
end

function Base.:(==)(d1::NED, d2::NED)
  @match (d1, d2) begin
    (Leaf(x1), Leaf(x2)) => x1[] == x2[]
    (Node(b1), Node(b2)) => b1 == b2
    _ => false
  end
end

@tests NonEmptyDtry.Type begin
  @test Node([:home => Leaf(Ref(1))]) isa NED
end

# TODO:

# - Base.getindex(t::NonEmptyDtry{A}, p::Path)::Union{Some{A}, Nothing}
# - Base.setindex!(t::NonEmptyDtry{A}, x::A, p::Path)
# - Base.map
# - Base.show

function getfromsorted(pairs::Vector{Pair{Symbol,X}}, n::Symbol)::Union{Some{X},Nothing} where {X}
  idx = searchsortedfirst(pairs, (n, nothing), by=first)
  if idx > length(pairs)
    nothing
  elseif first(pairs[idx]) == n
    Some(last(pairs[idx]))
  else
    nothing
  end
end

@tests getfromsorted begin
  pairs = [:foo => 1, :gar => 5]
  @test getfromsorted(pairs, :foo) == Some(1)
  @test isnothing(getfromsorted(pairs, :blah))
end

function getvalue(t::NED{A}, p::Path) where {A}
  @match t begin
    Leaf(x) =>
    if isempty(p)
      Some(x[]::A)
    else
      nothing
    end
    Node(branches) =>
    if isempty(p)
      nothing
    else
      @match getfromsorted(branches, head(p)) begin
        ::Nothing => nothing
        Some(t′) => getvalue(t′, tail(p))
      end
    end
  end
end
export getvalue

@tests getvalue begin
  t = Node([:home => Leaf(Ref(1))])
  @test getvalue(t, Path([:home])) == Some(1)
  @test isnothing(getvalue(t, Path([])))
  @test isnothing(getvalue(t, Path([:away])))
  @test isnothing(getvalue(t, Path([:home, :base])))
  @test getvalue(Leaf(Ref(2)), Path([])) == Some(2)
end

function insertsorted!(pairs::Vector{Pair{Symbol,X}}, x::X, n::Symbol) where {X}
  idx = searchsortedfirst(pairs, (n, nothing), by=first)
  if idx > length(pairs)
    push!(pairs, n => x)
  elseif first(pairs[idx]) == n
    error("already contains the key $n")
  else
    insert!(pairs, idx, n => x)
  end
end

@tests insertsorted! begin
  pairs = [:bar => 0, :foo => 1]
  insertsorted!(pairs, 2, :baz)
  @test getfromsorted(pairs, :baz) == Some(2)
  insertsorted!(pairs, 1, :zaz)
  @test getfromsorted(pairs, :zaz) == Some(1)
  @test_throws Exception insertsorted!(pairs, 0, :zaz)
end

function singleton(p::Path, v::A) where {A}
  if isempty(p)
    leaf(v)
  else
    node(head(p) => singleton(tail(p), v))
  end
end
export singleton

@tests singleton begin
  @test singleton(Path([]), 2) == leaf(2)
  @test singleton(Path([:a, :b]), 2) == node(:a => node(:b => leaf(2)))
end

function setvalue!(t::NED{A}, v::A, p::Path) where {A}
  @match t begin
    Leaf(x) =>
    if isempty(p)
      x[] = v
    else
      error("violated prefix-free invariant for Dtrys")
    end
    Node(branches) =>
    if isempty(p)
      error("violated prefix-free invariant for Dtrys")
    else
      @match getfromsorted(branches, head(p)) begin
        ::Nothing => insertsorted!(branches, singleton(tail(p), v), head(p))
        Some(t′) => setvalue!(t′, v, tail(p))
      end
    end
  end
end
export setvalue!

@tests setvalue! begin
  t = leaf(2)
  setvalue!(t, 1, Path([]))
  @test getvalue(t, Path([])) == Some(1)
  t = Node([:a => leaf(1)])
  setvalue!(t, 2, Path([:b, :a]))
  @test getvalue(t, Path([:b, :a])) == Some(2)
  @test_throws Exception setvalue!(t, 2, Path([:a, :b]))
end

@data Dtry{A} begin
  NonEmpty(NED{A})
  Empty
end

end # module Dtrys
