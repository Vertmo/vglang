#include <stdlib.h>
#include <stdio.h>
#include "game.c"

void f(void *entity) {
    printf("salut\n");
}

int main() {
    component$move$list = malloc(sizeof(component$move$list));
    component$move$list->move = f;

    int tick = 0;
    while(1) {
        printf("%d\n", tick);
        system$world$update(tick);
        tick++;
    }
}
