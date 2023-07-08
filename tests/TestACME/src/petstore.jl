module PetStore
abstract type Pet end
export makenoise, Dog
# TODO: handle macro + struct base.kwdef
struct Dog <: Pet
end
function makenoise(pet::Dog)
    print("Bow!")
end
end
