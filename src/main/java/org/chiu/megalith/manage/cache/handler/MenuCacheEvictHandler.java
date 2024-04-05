package org.chiu.megalith.manage.cache.handler;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.manage.cache.CacheEvictHandler;
import org.chiu.megalith.manage.entity.RoleEntity;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.chiu.megalith.manage.wrapper.RoleMenuWrapper;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.chiu.megalith.infra.lang.Const.HOT_MENUS;

@RequiredArgsConstructor
@Component
public class MenuCacheEvictHandler implements CacheEvictHandler {

    private final RoleRepository roleRepository;

    private final CacheKeyGenerator cacheKeyGenerator;

    @Override
    public boolean match(String prefix) {
        return HOT_MENUS.getInfo().equals(prefix);
    }

    @SneakyThrows
    @Override
    public Set<String> handle(String prefix) {
        List<RoleEntity> allRoles = roleRepository.findAll();
        List<String> roleList = allRoles.stream()
                .map(RoleEntity::getCode)
                .toList();

        Method method = RoleMenuWrapper.class.getMethod("getCurrentRoleNav", String.class);
        Set<String> set = new HashSet<>();
        roleList.forEach(role -> set.add(cacheKeyGenerator.generateKey(method, role)));
        return set;
    }
}
