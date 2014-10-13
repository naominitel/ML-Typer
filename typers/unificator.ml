module 
class type unificator = object
    method add_constraints : constr list -> unit
    method return : ty -> ty
    method get_result : ty -> ty
end

class late_unificator: unificator = object
    let equ_sys = Stack.create ()

    method add_constraints constrs =
        List.iter (Stack.push equ_sys) constrs

    method return ty =
        ty

    method get_result ty =
        let alist = Types.Unif.unify equ_sys in
        let substs = Types.Subst.from_alist alist in
        Types.Subst.apply substs ty
end

class subsitutions: unificator = object
    let bindings = Types.Subst.empty () in

    method add_constraints constrs =
        Types.Substs.unify bindings constrs

    method return ty =
        Types.Substs.apply bindings ty

    method get_result ty =
        ty
end
