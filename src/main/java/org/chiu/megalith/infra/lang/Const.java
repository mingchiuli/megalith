package org.chiu.megalith.infra.lang;


/**
 * @author mingchiuli
 * @create 2021-12-14 11:58 AM
 */
public enum Const {

    GRANT_TYPE_EMAIL("email"),

    EMAIL_CODE("email_code"),
    
    SMS_CODE("sms_code"),

    DAY_VISIT("{visit_record}_day"),

    WEEK_VISIT("{visit_record}_week"),

    MONTH_VISIT("{visit_record}_month"),

    YEAR_VISIT("{visit_record}_year"),

    LOGIN_USER_CACHE("login:"),

    GRANT_TYPE_PASSWORD("password"),

    GRANT_TYPE_PHONE("phone"),

    CAPTCHA_KEY("captcha:"),

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

    JSON_WEB_TOKEN("jwt"),

    ROLE_TOKEN_TOOL("ROLE_token_tool"),

    HOT_READ("hot_read"),
    
    BLOG_CONTENT("content");

    private final String info;

    public String getInfo() {
        return info;
    }

    Const(String info) {
        this.info = info;
    }

}

