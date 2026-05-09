#include <tree_sitter/api.h>

void ts_tree_root_node_p(const TSTree *tree, TSNode *out) {
    *out = ts_tree_root_node(tree);
}

bool ts_node_has_error_p(const TSNode *node) {
    return ts_node_has_error(*node);
}

const char *ts_node_type_p(const TSNode *node) {
    return ts_node_type(*node);
}

void ts_node_start_point_p(const TSNode *node, TSPoint *out) {
    *out = ts_node_start_point(*node);
}

uint32_t ts_node_child_count_p(const TSNode *node) {
    return ts_node_child_count(*node);
}

void ts_node_child_p(const TSNode *node, uint32_t index, TSNode *out) {
    *out = ts_node_child(*node, index);
}

bool ts_node_is_null_p(const TSNode *node) {
    return ts_node_is_null(*node);
}

bool ts_node_is_named_p(const TSNode *node) {
    return ts_node_is_named(*node);
}

uint32_t ts_node_start_byte_p(const TSNode *node) {
    return ts_node_start_byte(*node);
}

uint32_t ts_node_end_byte_p(const TSNode *node) {
    return ts_node_end_byte(*node);
}

void ts_query_cursor_exec_p(TSQueryCursor *cursor, const TSQuery *query, const TSNode *node) {
    ts_query_cursor_exec(cursor, query, *node);
}

bool ts_query_cursor_next_capture_p(TSQueryCursor *cursor, TSNode *out_node, uint32_t *out_capture_index) {
    TSQueryMatch match;
    uint32_t capture_index = 0;
    bool ok = ts_query_cursor_next_capture(cursor, &match, &capture_index);
    if (!ok) {
        return false;
    }
    *out_node = match.captures[capture_index].node;
    *out_capture_index = match.captures[capture_index].index;
    return true;
}
