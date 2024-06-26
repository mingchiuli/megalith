package org.chiu.megalith.user.cache.handler;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.user.cache.CacheEvictHandler;
import org.chiu.megalith.user.entity.RoleEntity;
import org.chiu.megalith.user.repository.RoleRepository;
import org.chiu.megalith.user.wrapper.RoleAuthorityWrapper;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@RequiredArgsConstructor
@Component
@SuppressWarnings("unchecked")
public class BatchAuthorityCacheEvictHandler extends CacheEvictHandler {

    private final RoleRepository roleRepository;

    private final CacheKeyGenerator cacheKeyGenerator;

    @SneakyThrows
    @Override
    public Set<String> handle(Object[] args) {
        Object roleIds = args[0];

        List<RoleEntity> roleEntities = roleRepository.findAllById((Iterable<Long>) roleIds);
        List<String> roleCodes = roleEntities.stream().map(RoleEntity::getCode).toList();

        Method method = RoleAuthorityWrapper.class.getMethod("getAuthoritiesByRoleCode", String.class);
        Set<String> set = new HashSet<>();
        roleCodes.forEach(role -> set.add(cacheKeyGenerator.generateKey(method, role)));

        return set;
    }
}
