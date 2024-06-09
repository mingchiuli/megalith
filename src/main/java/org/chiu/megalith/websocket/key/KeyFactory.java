package org.chiu.megalith.websocket.key;

import java.util.Objects;

import static org.chiu.megalith.infra.lang.Const.TEMP_EDIT_BLOG;

public class KeyFactory {

    private KeyFactory() {}

    public static String createPushContentIdentityKey(Long userId, Long blogId) {
        return Objects.isNull(blogId) ?
                userId.toString() :
                userId + "/" + blogId;
    }

    public static String createBlogEditRedisKey(Long userId, Long blogId) {
        return Objects.isNull(blogId) ?
                TEMP_EDIT_BLOG.getInfo() + userId :
                TEMP_EDIT_BLOG.getInfo() + userId + ":" + blogId;
    }
}
