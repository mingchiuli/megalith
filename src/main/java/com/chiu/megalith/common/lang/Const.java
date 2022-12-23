package com.chiu.megalith.common.lang;


/**
 * @author mingchiuli
 * @create 2021-12-14 11:58 AM
 */
public enum Const {

    CAPTCHA_KEY("captcha:"),

    TOKEN("token"),

    READ_RECENT("blogReadRecent:"),

    HOT_BLOGS_PATTERN("hot_blogs*"),

    CO_PREFIX("co_blogId:"),

    CO_NUM_PREFIX("co_num:"),

    QUERY_ALL_DELETED(":blog:*"),

    QUERY_DELETED(":blog:"),

    READ_TOKEN("read_token"),

    HOT_BLOGS("hot_blogs"),

    HOT_BLOG("hot_blog"),

    BLOG_STATUS("blog_status"),

    CONSUME_MONITOR("consume:"),

    BLOOM_FILTER_BLOG("bloom_filter_blog"),

    BLOOM_FILTER_PAGE("bloom_filter_page"),

    BLOOM_FILTER_YEAR_PAGE("bloom_filter_page_"),

    YEARS("years"),

    BLOOM_FILTER_YEARS("bloom_filter_years"),

    BLOG_PAGE_SIZE("5"),

    WEB_PAGE_SIZE("9"),

    JSON_WEB_TOKEN("jwt"),

    TOKEN_TOOL("token_tool"),

    ROLE_TOKEN_TOOL("ROLE_token_tool");


    private final String msg;

    public String getMsg() {
        return msg;
    }

    Const(String msg) {
        this.msg = msg;
    }
}

