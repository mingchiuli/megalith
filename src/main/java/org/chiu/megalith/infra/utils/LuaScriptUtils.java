package org.chiu.megalith.infra.utils;

import org.springframework.data.redis.core.script.RedisScript;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2023-03-30 8:42 pm
 */
public class LuaScriptUtils {
    
    private LuaScriptUtils() {}

    public static final RedisScript<Void> statisticLua = RedisScript.of(
            "redis.call('pfadd', KEYS[1], ARGV[1]);" +
                    "redis.call('pfadd', KEYS[2], ARGV[1]);" +
                    "redis.call('pfadd', KEYS[3], ARGV[1]);" +
                    "redis.call('pfadd', KEYS[4], ARGV[1]);");

    public static final RedisScript<List> getVisitLua = RedisScript.of(
            "local daySize = redis.call('pfcount', KEYS[1]);" +
                    "local weekSize = redis.call('pfcount', KEYS[2]);" +
                    "local monthSize = redis.call('pfcount', KEYS[3]);" +
                    "local yearSize = redis.call('pfcount', KEYS[4]);" +
                    "local resp = {};" +
                    "resp[1] = daySize;" +
                    "resp[2] = weekSize;" +
                    "resp[3] = monthSize;" +
                    "resp[4] = yearSize;" +
                    "return resp;",
            List.class);

    public static final RedisScript<Long> emailOrPhoneLua = RedisScript.of(
            "local ttl =  redis.call('ttl', KEYS[1]);" +
                    "if (ttl == -2) then return 0 end;" +
                    "redis.call('hincrby', KEYS[1], ARGV[1], 1);" +
                    "redis.call('expire', KEYS[1], ttl);" +
                    "return ttl;",
            Long.class);

    public static final RedisScript<Void> passwordLua = RedisScript.of(
            "redis.call('ltrim', KEYS[1], ARGV[1], ARGV[2]);" +
                    "redis.call('rpush', KEYS[1], ARGV[3]);" +
                    "redis.call('expire', KEYS[1], ARGV[4])");

    public static final RedisScript<Void> sendBlogToTempLua = RedisScript.of(
            "redis.call('hset', KEYS[1], ARGV[1], ARGV[3]);" +
                    "redis.call('hset', KEYS[1], ARGV[2], ARGV[4]);" +
                    "redis.call('expire', KEYS[1], ARGV[5]);");

    public static final RedisScript<Void> setBlogDeleteLua = RedisScript.of(
                    "redis.call('rpush', KEYS[1], ARGV[1]);" +
                    "redis.call('expire', KEYS[1], ARGV[2]);");

    public static final RedisScript<Long> countYears = RedisScript.of(
        "return redis.call('bitcount', KEYS[1]);", 
        Long.class);

    public static final RedisScript<List> listDeletedRedisScript = RedisScript.of(
            "redis.call('ltrim', KEYS[1], ARGV[1], ARGV[2]);" +
                    "local total = redis.call('llen', KEYS[1]);" +
                    "local resp = redis.call('lrange', KEYS[1], ARGV[4], ARGV[3] + ARGV[4]);" +
                    "local len = #resp;" +
                    "resp[len + 1] = total;" +
                    "return resp",
            List.class);

    public static final RedisScript<String> recoverDeletedScript = RedisScript.of(
            "local str =  redis.call('lindex', KEYS[1], ARGV[1]);" +
                    "redis.call('lrem', KEYS[1], '1', str)" +
                    "return str;",
            String.class);
}
