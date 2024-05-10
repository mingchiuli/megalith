package org.chiu.megalith.authority.service.impl;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.authority.convertor.RoleAuthorityEntityConvertor;
import org.chiu.megalith.authority.entity.AuthorityEntity;
import org.chiu.megalith.authority.entity.RoleAuthorityEntity;
import org.chiu.megalith.authority.repository.AuthorityRepository;
import org.chiu.megalith.authority.repository.RoleAuthorityRepository;
import org.chiu.megalith.authority.service.RoleAuthorityService;
import org.chiu.megalith.authority.vo.RoleAuthorityVo;
import org.chiu.megalith.authority.wrapper.RoleAuthorityWrapper;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;


@Service
@RequiredArgsConstructor
public class RoleAuthorityServiceImpl implements RoleAuthorityService {

   private final RoleAuthorityWrapper roleAuthorityWrapper;

   private final AuthorityRepository authorityRepository;

    private final RoleAuthorityRepository roleAuthorityRepository;

    @Override
    public List<String> getAuthoritiesByRoleCode(String RoleCode) {
        return roleAuthorityWrapper.getAuthoritiesByRoleCode(RoleCode);
    }

    /**
     * @param roleId
     * @param authorityIds
     */
    @Override
    public void saveAuthority(Long roleId, List<Long> authorityIds) {
        List<RoleAuthorityEntity> roleAuthorityEntities = RoleAuthorityEntityConvertor.convert(roleId, authorityIds);
        roleAuthorityWrapper.saveAuthority(roleId, new ArrayList<>(roleAuthorityEntities));
    }

    @Override
    public List<RoleAuthorityVo> getAuthoritiesInfo(Long roleId) {
        List<AuthorityEntity> allAuthorityEntities = authorityRepository.findByStatus(NORMAL.getCode());
        List<RoleAuthorityEntity> authorityEntities = roleAuthorityRepository.findByRoleId(roleId);

        List<Long> ids = authorityEntities.stream()
                .map(RoleAuthorityEntity::getAuthorityId)
                .toList();
        List<RoleAuthorityVo> roleAuthorityVos = new ArrayList<>();

        allAuthorityEntities.forEach(item -> roleAuthorityVos
                .add(RoleAuthorityVo.builder()
                        .authorityId(item.getId())
                        .code(item.getCode())
                        .check(ids.contains(item.getId()))
                        .build()));
        return roleAuthorityVos;
    }
}
