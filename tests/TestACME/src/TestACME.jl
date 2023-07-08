module TestACME
using UUIDs
include("petstore.jl")
using .PetStore
function main()
    @info "Hello customer $(uuid4())"
    PetStore.makenoise(Dog())
end


end # module
