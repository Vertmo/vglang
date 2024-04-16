(** Compilation unit (not really: there is no real separate compilation?) *)
type file =
  | Entity of Entity.entity
  | Component of Component.component
  | System of System.system
  | Scene of Scene.scene
