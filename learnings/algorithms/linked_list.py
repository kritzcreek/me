#!/usr/bin/python
#
# A simple singly-linked list. Yay.


class Node:
    def __init__(self, value, next = None):
        self.value = value
        self.next = next

a_list = Node("a", Node("b", Node("c", Node("d"))))

a_list.next.next.value == 'c' # True
