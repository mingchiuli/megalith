package org.chiu.megalith.authority.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.authority.entity.AuthorityEntity;
import org.chiu.megalith.authority.repository.AuthorityRepository;
import org.chiu.megalith.authority.req.AuthorityEntityReq;
import org.chiu.megalith.authority.service.AuthorityService;
import org.chiu.megalith.authority.vo.AuthorityVo;
import org.chiu.megalith.authority.wrapper.AuthorityWrapper;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.chiu.megalith.authority.convertor.AuthorityVoConvertor;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.NO_FOUND;

@Service
@RequiredArgsConstructor
public class AuthorityServiceImpl implements AuthorityService {

    private final AuthorityRepository authorityRepository;

    private final AuthorityWrapper authorityWrapper;

    private final ObjectMapper objectMapper;

    @Override
    public List<AuthorityVo> findAll() {
        List<AuthorityEntity> authorityEntities = authorityRepository.findAll();
        return AuthorityVoConvertor.convert(authorityEntities);
    }

    @Override
    public AuthorityVo findById(Long id) {
        AuthorityEntity authorityEntity = authorityRepository.findById(id)
                .orElseThrow(() -> new MissException(NO_FOUND));
        return AuthorityVoConvertor.convert(authorityEntity);
    }

    @Override
    public void saveOrUpdate(AuthorityEntityReq req) {

        Long id = req.getId();
        AuthorityEntity authorityEntity;

        if (Objects.nonNull(id)) {
            authorityEntity = authorityRepository.findById(id)
                    .orElseThrow(() -> new MissException(NO_FOUND));
        } else {
            authorityEntity = new AuthorityEntity();
        }

        BeanUtils.copyProperties(req, authorityEntity);
        authorityWrapper.save(authorityEntity);
    }

    @Override
    public void deleteAuthorities(List<Long> ids) {
        authorityWrapper.deleteAllById(ids);
    }

    @SneakyThrows
    @Override
    public void download(HttpServletResponse response) {
        ServletOutputStream outputStream = response.getOutputStream();
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());

        List<AuthorityEntity> authorities = authorityRepository.findAll();
        byte[] bytes = objectMapper.writeValueAsBytes(authorities);
        outputStream.write(bytes);
        outputStream.flush();
        outputStream.close();
    }
}
