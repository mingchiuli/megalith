package com.chiu.megalith.authentication.component;

import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;

import java.util.Optional;


@Component
@RequiredArgsConstructor
public class UserDetailServiceImpl implements UserDetailsService {

	private final UserRepository userRepository;

	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		Optional<UserEntity> sysUser = userRepository.findByUsername(username);

		UserEntity user = sysUser.
				orElseThrow(() -> new UsernameNotFoundException("username not exist"));

		//通过User去自动比较用户名和密码
		return new User(user.getUsername(),
				user.getPassword(),
				true,
				true,
				true,
				user.getStatus() == 0,
				AuthorityUtils.createAuthorityList("ROLE_" + user.getRole()));
	}
}
