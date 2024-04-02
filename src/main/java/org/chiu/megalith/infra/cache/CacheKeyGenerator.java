package org.chiu.megalith.infra.cache;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.blog.wrapper.BlogWrapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * @author mingchiuli
 * @create 2023-04-02 11:12 pm
 */
@Component
@RequiredArgsConstructor
public class CacheKeyGenerator {

    private final ObjectMapper objectMapper;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    @SneakyThrows
    public String generateKey(Class<?> declaringType,
                              String methodName,
                              Class<?>[] parameterTypes,
                              Object[] args) {

        var params = new StringBuilder();
        for (Object arg : args) {
            if (Objects.nonNull(arg)) {
                params.append("::");
                if (arg instanceof String) {
                    params.append(arg);
                } else {
                    params.append(objectMapper.writeValueAsString(arg));
                }
            }
        }

        String className = declaringType.getSimpleName();
        Method method = declaringType.getMethod(methodName, parameterTypes);
        var annotation = method.getAnnotation(Cache.class);
        String prefix = null;
        if (Objects.nonNull(annotation)) {
            prefix = annotation.prefix().getInfo();
        }

        return StringUtils.hasLength(prefix) ?
                prefix + "::" + className + "::" + methodName + params :
                className + "::" + methodName + params;
    }

    public Set<String> generateHotBlogsKeys(Integer year, Long count, Long countYear) {
        Set<String> keys = new HashSet<>();
        long pageNo = count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1;
        long pageYearNo = countYear % blogPageSize == 0 ? countYear / blogPageSize : countYear / blogPageSize + 1;

        for (long i = 1; i <= pageNo; i++) {
            String key = generateKey(BlogWrapper.class, "findPage", new Class[]{Integer.class, Integer.class}, new Object[]{i, Integer.MIN_VALUE});
            keys.add(key);
        }

        for (long i = 1; i <= pageYearNo; i++) {
            String key = generateKey(BlogWrapper.class, "findPage", new Class[]{Integer.class, Integer.class}, new Object[]{i, year});
            keys.add(key);
        }
        return keys;
    }

    public Set<String> generateBlogKey(long count, long countBefore, long countYear, long countYearBefore, Integer year) {
        Set<String> keys = new HashSet<>();
        long totalPageNo = count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1;
        long pageBeforeNo = countBefore % blogPageSize == 0 ? countBefore / blogPageSize + 1 : countBefore / blogPageSize;

        long totalYearPageNo = countYear % blogPageSize == 0 ? countYear / blogPageSize : countYear / blogPageSize + 1;
        long pageYearBeforeNo = countYearBefore % blogPageSize == 0 ? countYearBefore / blogPageSize + 1 : countYearBefore / blogPageSize;

        for (long i = pageBeforeNo; i <= totalPageNo; i++) {
            String key = generateKey(BlogWrapper.class, "findPage", new Class[]{Integer.class, Integer.class}, new Object[]{i, Integer.MIN_VALUE});
            keys.add(key);
        }

        for (long i = pageYearBeforeNo; i <= totalYearPageNo; i++) {
            String key = generateKey(BlogWrapper.class, "findPage", new Class[]{Integer.class, Integer.class}, new Object[]{i, year});
            keys.add(key);
        }
        return keys;
    }
}
