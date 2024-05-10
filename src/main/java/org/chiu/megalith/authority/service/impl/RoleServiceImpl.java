package org.chiu.megalith.authority.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.authority.convertor.RoleEntityVoConvertor;
import org.chiu.megalith.authority.entity.RoleEntity;
import org.chiu.megalith.authority.repository.RoleRepository;
import org.chiu.megalith.authority.service.RoleService;
import org.chiu.megalith.authority.req.RoleEntityReq;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.authority.vo.RoleEntityVo;
import org.chiu.megalith.authority.wrapper.RoleWrapper;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.ROLE_NOT_EXIST;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:26 am
 */
@Service
@RequiredArgsConstructor
public class RoleServiceImpl implements RoleService {

    private final RoleRepository roleRepository;

    private final RoleWrapper roleWrapper;

    private final ObjectMapper objectMapper;

    @Override
    public RoleEntityVo info(Long id) {
        RoleEntity roleEntity = roleRepository.findById(id)
                .orElseThrow(() -> new MissException(ROLE_NOT_EXIST));

        return RoleEntityVoConvertor.convert(roleEntity);
    }

    @Override
    public PageAdapter<RoleEntityVo> getPage(Integer currentPage, Integer size) {
        var pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<RoleEntity> page = roleRepository.findAll(pageRequest);

        return RoleEntityVoConvertor.convert(page);
    }

    @Override
    public void saveOrUpdate(RoleEntityReq roleReq) {

        Long id = roleReq.getId();
        RoleEntity roleEntity;

        if (Objects.nonNull(id)) {
            roleEntity = roleRepository.findById(id)
                    .orElseThrow(() -> new MissException(ROLE_NOT_EXIST));
        } else {
            roleEntity = new RoleEntity();
        }

        BeanUtils.copyProperties(roleReq, roleEntity);
        roleWrapper.save(roleEntity);
    }

    @Override
    public void delete(List<Long> ids) {
        roleWrapper.delete(ids);
    }

    @Override
    public List<RoleEntityVo> getValidAll() {
        List<RoleEntity> entities = roleRepository.findByStatus(StatusEnum.NORMAL.getCode());
        return RoleEntityVoConvertor.convert(entities);
    }

    @SneakyThrows
    @Override
    public void download(HttpServletResponse response) {
        ServletOutputStream outputStream = response.getOutputStream();
        response.setCharacterEncoding("UTF-8");

        List<RoleEntity> roles = roleRepository.findAll();
        byte[] bytes = objectMapper.writeValueAsBytes(roles);
        outputStream.write(bytes);
        outputStream.flush();
        outputStream.close();
    }
}
