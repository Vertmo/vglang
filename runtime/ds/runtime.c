#include "runtime.h"

// TODO we need a free list or something
// TODO problem: how to use different / same gfx accros multiple entities depending on the case?
entity_t *entity_create(int *entry, int nbTiles, int x, int y) {
  if(*entry+nbTiles >= 128) return NULL;
  entity_t *ent = malloc(sizeof(entity_t));
  ent->x = x; ent->y = y;

  // Allocate entries
  ent->entries = malloc(nbTiles * sizeof(int));
  ent->gfx = malloc(nbTiles * sizeof(u16 *));
  for(int i = 0; i < nbTiles; i++) {
    ent->entries[i] = *entry++;
  }

  return ent;
}

void entity_draw_gfx(OamState *oam, int nbTiles, int palette, int size, int format, entity_t *entity) {
  for(int i = 0; i < nbTiles; i++) {
    oamSet(oam, entity->entries[i],
           entity->x, entity->y, 1,
           palette,
           size, format, entity->gfx[i], 32,
           false, false, false, false, false);
  }
}

void add_entity(entity_t* ent, entity_list_t **list) {
  entity_list_t *l = malloc(sizeof(entity_list_t));
  l->ent = ent;
  l->next = l;
  *list = l;
}
