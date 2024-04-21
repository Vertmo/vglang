#ifndef RUNTIME_H_
#define RUNTIME_H_

#include <nds.h>

// Dynamic entity data: entry, position
typedef struct {
  int *entries;
  u16 **gfx; // Current gfx for this entity
  int x, y;
} entity_t;

entity_t *entity_create(int *entry, int nbTiles, int x, int y);
void entity_draw_gfx(OamState *oam, int nbTiles, int palette, int size, int format, entity_t *entity);

// Linked list of entities
typedef struct __entity_list_t {
  entity_t *ent;
  struct __entity_list_t *next;
} entity_list_t;

void add_entity(entity_t* ent, entity_list_t **list);

#endif // RUNTIME_H_
