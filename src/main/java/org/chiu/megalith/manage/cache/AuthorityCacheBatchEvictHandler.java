package org.chiu.megalith.manage.cache;


import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.infra.cache.CacheBatchEvictHandler;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.chiu.megalith.manage.wrapper.RoleAuthorityWrapper;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.chiu.megalith.infra.lang.Const.HOT_AUTHORITIES;

@Component
@RequiredArgsConstructor
public class AuthorityCacheBatchEvictHandler implements CacheBatchEvictHandler {

    private final RoleRepository roleRepository;

    private final CacheKeyGenerator cacheKeyGenerator;

    @Override
    public boolean match(String prefix) {
        return HOT_AUTHORITIES.getInfo().equals(prefix);
    }

    @SneakyThrows
    @Override
    public Set<String> handle(String prefix) {
        List<String> roleList = roleRepository.findAllCodes();

        Method method = RoleAuthorityWrapper.class.getMethod("getAuthoritiesByRoleCode", String.class);
        Set<String> set = new HashSet<>();
        roleList.forEach(role -> set.add(cacheKeyGenerator.generateKey(method, role)));
        return set;
    }
}
