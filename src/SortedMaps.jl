module SortedMaps
using Test
using AssociatedTests

struct SortedMap{K,V}
    pairs::Vector{Pair{K,V}}
    function SortedMap{K,V}(values::Vector{Pair{K,V}}, presorted=false) where {K,V}
        if presorted
            new(values)
        else
            new(sort(values, by=first))
        end
    end
    function SortedMap(values::Vector{Pair{K,V}}, presorted=false) where {K,V}
        SortedMap{K,V}(values, presorted)
    end
    function SortedMap{K,V}(pairs...) where {K,V}
        SortedMap{K,V}([pairs...])
    end
    function SortedMap(pairs...)
        SortedMap([pairs...])
    end
end
export SortedMap

function Base.:(==)(m1::SortedMap{K,V}, m2::SortedMap{K,V}) where {K,V}
    m1.pairs == m2.pairs
end

function Base.getindex(m::SortedMap{K,V}, n::K)::Union{Some{V},Nothing} where {K,V}
    idx = searchsortedfirst(m.pairs, (n, nothing), by=first)
    if idx > length(m.pairs)
        return nothing
    end
    (n′, v) = m.pairs[idx]
    if n == n′
        Some(v)
    else
        nothing
    end
end

@tests getindex begin
    pairs = SortedMap(:foo => 1, :gar => 5)
    @test pairs[:foo] == Some(1)
    @test isnothing(pairs[:bar])
end

function Base.setindex!(m::SortedMap{K,V}, x::V, n::Symbol) where {K,V}
    idx = searchsortedfirst(m.pairs, (n, nothing), by=first)
    if idx > length(m.pairs)
        push!(m.pairs, n => x)
    elseif first(m.pairs[idx]) == n
        error("already contains the key $n")
    else
        insert!(m.pairs, idx, n => x)
    end
end

@tests setindex! begin
    m = SortedMap(:bar => 0, :foo => 1)
    m[:baz] = 2
    @test m[:baz] == Some(2)
    m[:zaz] = 1
    @test m[:zaz] == Some(1)
    @test_throws Exception m[:zaz] = 2
end

function Base.pairs(m::SortedMap)
    m.pairs
end

function Base.iterate(m::SortedMap)
    Base.iterate(m.pairs)
end

function Base.iterate(m::SortedMap, i)
    Base.iterate(m.pairs, i)
end

Base.keys(m::SortedMap) = first.(m.pairs)

end
