package org.chiu.megalith.infra.lang;

import lombok.Getter;

/**
 * @author mingchiuli
 * @create 2021-12-14 11:58 AM
 */
@Getter
public enum Const {

    PARAGRAPH_PREFIX("para::"),

    PARAGRAPH_SPLITTER("\n\n"),

    TEMP_EDIT_BLOG("temp_edit_blog:"),

    GRANT_TYPE_EMAIL("email"),

    EMAIL_CODE("email_code"),

    PHONE_CODE("phone_code"),
    
    SMS_CODE("sms_code"),

    DAY_VISIT("{visit_record}_day"),

    WEEK_VISIT("{visit_record}_week"),

    MONTH_VISIT("{visit_record}_month"),

    YEAR_VISIT("{visit_record}_year"),

    EMAIL_KEY("email_validation:"),

    PHONE_KEY("phone_validation:"),

    PASSWORD_KEY("password_validation:"),

    READ_RECENT("blogReadRecent:"),

    HOT_BLOGS_PATTERN("hot_blogs*"),

    COOP_PREFIX("coop_blogId:"),

    CO_NUM_PREFIX("co_num:"),

    QUERY_DELETED("del_blog_user:"),

    READ_TOKEN("read_token:"),

    HOT_BLOGS("hot_blogs"),

    HOT_BLOG("hot_blog"),

    BLOG_STATUS("blog_status"),

    CONSUME_MONITOR("consume:"),

    BLOOM_FILTER_BLOG("bloom_filter_blog"),

    BLOOM_FILTER_BLOG_STATUS("bloom_filter_blog_status"),

    BLOOM_FILTER_PAGE("bloom_filter_page"),

    BLOOM_FILTER_YEAR_PAGE("bloom_filter_page_"),

    BLOOM_FILTER_YEARS("bloom_filter_years"),

    HOT_READ("hot_read"),
    
    BLOG_CONTENT("content"),

    TOKEN_PREFIX("Bearer "),

    ROLE_PREFIX("ROLE_"),

    HOT_AUTHORITIES("hot_authorities"),

    HOT_MENUS_AND_BUTTONS("hot_menus_and_buttons"),
    
    A_WEEK("604899"),

    BLOCK("block"),

    BLOCK_USER("block_user:"),

    REGISTER_PREFIX("register_prefix:"),

    USER("user"),

    CACHE_EVICT_FANOUT_EXCHANGE("cache.evict.fanout.exchange"),

    USER_DISABLED("用户已注销");



    private final String info;

    Const(String info) {
        this.info = info;
    }

}

