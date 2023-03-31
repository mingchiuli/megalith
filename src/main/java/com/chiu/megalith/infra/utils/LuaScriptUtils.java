package com.chiu.megalith.infra.utils;

import org.springframework.data.redis.core.script.RedisScript;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2023-03-30 8:42 pm
 */
public class LuaScriptUtils {

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
                    "table.insert(resp, daySize);" +
                    "table.insert(resp, weekSize);" +
                    "table.insert(resp, monthSize);" +
                    "table.insert(resp, yearSize);" +
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

    public static final RedisScript<Void> sendUserToSessionLua = RedisScript.of(
            "redis.call('hset', KEYS[1], ARGV[1], ARGV[2]);" +
            "redis.call('expire', KEYS[1], ARGV[3]);");
}
